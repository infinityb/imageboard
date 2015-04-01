extern crate iron;
extern crate mount;
extern crate time;
extern crate rustc_serialize;

use std::io::{Read, Write};
use std::sync::RwLock;
use std::collections::{HashMap, BTreeMap};

use rustc_serialize::json::{self, Json, ToJson};
use time::precise_time_ns;

use iron::status;
use iron::headers::{self, ContentType};
use iron::prelude::{
    Chain, Iron, IronResult, IronError,
    Request, Response, Set,
};
use iron::typemap::Key;
use iron::middleware::{AfterMiddleware, BeforeMiddleware, Handler};
use iron::method::Method;

use mount::Mount;

macro_rules! unwrap_opt_return_val(
    ($e:expr, $r:expr) => {{
        match $e {
            Some(v) => v,
            None => return $r,
        }
    }}
);

fn service_unavailable() -> IronResult<Response> {
    let mut response = Response::new();
    response.set_mut(status::InternalServerError);
    response.headers.set(ContentType("text/plain".parse().unwrap()));
    response.set_mut("Sorry, we are down for maintenance!");
    Ok(response)
}

fn json_wrap(key: &str, json: Json) -> Json {
    let mut doc = BTreeMap::new();
    doc.insert(key.to_string(), json);
    Json::Object(doc)
}

struct BoardInfo {
    // e.g. ``prog``
    id: String,
   
    // e.g. ``Programming``
    title: String,
}

impl BoardInfo {
    pub fn new(id: &str, title: &str) -> BoardInfo {
        BoardInfo {
            id: id.to_string(),
            title: title.to_string(),
        }
    }
}

#[derive(RustcDecodable, RustcEncodable, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
struct PostId(pub u64);

impl ToJson for PostId {
    fn to_json(&self) -> Json {
        Json::U64(self.0)
    }
}

struct Thread {
    id: PostId,
    posts: Vec<PostId>,
}

impl Thread {
    fn new(op: PostId) -> Thread {
        Thread {
            id: op,
            posts: vec![op],
        }
    }

    fn add_post(&mut self, post: &Post) {
        self.posts.push(post.id);
    }
}

#[derive(RustcDecodable)]
struct PostReplyRequest {
    message: String,
}

#[derive(RustcDecodable)]
struct PostNewThreadRequest {
    message: String,
}

#[derive(RustcEncodable, Debug)]
struct Post {
    id: PostId,
    message: String,
}

impl Post {
    fn from_reply(id: PostId, request: PostReplyRequest) -> Post {
        Post {
            id: id,
            message: request.message,
        }
    }

    fn from_new_thread(id: PostId, request: PostNewThreadRequest) -> Post {
        Post {
            id: id,
            message: request.message,
        }
    }
}

impl ToJson for Post {
    fn to_json(&self) -> Json {
        let mut doc = BTreeMap::new();
        doc.insert("id".to_string(), self.id.to_json());
        doc.insert("message".to_string(), self.message.to_json());
        Json::Object(doc)
    }
}


struct Board {
    min_post_id: PostId,
    board: BoardInfo,
    // thread_order: BTreeMap<SteadyTime, PostId>,
    threads: HashMap<PostId, Thread>,
    posts: HashMap<PostId, Post>,
}

enum ReplyError {
    NoThread,
}

enum NewThreadError {}

impl Board {
    fn new(board: BoardInfo) -> Board {
        Board {
            min_post_id: PostId(0),
            board: board,
            threads: HashMap::new(),
            posts: HashMap::new(),
        }
    }

    fn allocate_post_id(&mut self) -> PostId {
        self.min_post_id.0 += 1;
        self.min_post_id
    }

    fn post_reply(&mut self, thread_id: PostId, request: PostReplyRequest) -> Result<(), ReplyError> {
        let new_post = Post::from_reply(self.allocate_post_id(), request);
        let thread = unwrap_opt_return_val!(self.threads.get_mut(&thread_id), Err(ReplyError::NoThread));
        thread.add_post(&new_post);
        self.posts.insert(new_post.id, new_post);
        Ok(())
    }

    fn post_new_thread(&mut self, request: PostNewThreadRequest) -> Result<(), NewThreadError> {
        let new_post = Post::from_new_thread(self.allocate_post_id(), request);
        self.threads.insert(new_post.id, Thread::new(new_post.id));
        self.posts.insert(new_post.id, new_post);
        Ok(())
    }

    fn threads(&self) -> Json {
        let mut posts: Vec<Json> = Vec::new();
        for thread in self.threads.values() {
            if let Some(ref op_post) = self.posts.get(&thread.id) {
                posts.push(op_post.to_json());
            }
        }
        Json::Array(posts)
    }

    fn thread(&self, thread_id: PostId) -> Json {
        let mut posts: Vec<Json> = Vec::new();
        if let Some(ref thread_posts) = self.threads.get(&thread_id).and_then(|th| Some(&th.posts)) {
            for post in thread_posts.iter() {
                if let Some(ref post) = self.posts.get(post) {
                    posts.push(post.to_json());
                }                
            }
        }
        Json::Array(posts)
    }
}

// PostReplyRequest
// PostNewThreadRequest

struct BoardHandler {
    board: RwLock<Board>,
}

impl BoardHandler {
    fn new(board: Board) -> BoardHandler {
        BoardHandler { board: RwLock::new(board) }
    }

    fn handle_get(&self, thread_id: PostId, req: &mut Request) -> IronResult<Response> {
        let threads_json = match self.board.read() {
            Ok(read_guard) => json_wrap("posts", read_guard.thread(thread_id)),
            Err(_) => return service_unavailable(),
        };

        let reader = format!("{}", threads_json.pretty()).into_bytes();
        let mut response = Response::new();
        response.set_mut(status::Ok);
        response.headers.set(ContentType("application/json".parse().unwrap()));

        response.set_mut(reader);
        Ok(response)
    }

    fn handle_collection_get(&self, req: &mut Request) -> IronResult<Response> {
        let threads_json = match self.board.read() {
            Ok(read_guard) => json_wrap("threads", read_guard.threads()),
            Err(_) => return service_unavailable(),
        };

        let reader = format!("{}", threads_json.pretty()).into_bytes();
        let mut response = Response::new();
        response.set_mut(status::Ok);
        response.headers.set(ContentType("application/json".parse().unwrap()));

        response.set_mut(reader);
        Ok(response)
    }

    fn handle_put(&self, thread_id: PostId, req: &mut Request) -> IronResult<Response> {
        let mut body_buf = String::new();
        if let Err(_) = req.body.read_to_string(&mut body_buf) {
            return service_unavailable();
        }

        let new_thread_request = match json::decode(&body_buf) {
            Ok(new_thread_request) => new_thread_request,
            Err(_) => return service_unavailable(),
        };

        let post_result = match self.board.write() {
            Ok(mut write_guard) => write_guard.post_reply(thread_id, new_thread_request),
            Err(_) => return service_unavailable(),
        };

        if let Err(_) = post_result {
            return service_unavailable()
        }

        let mut response = Response::new();
        response.set_mut(status::Ok);
        response.headers.set(ContentType("text/plain".parse().unwrap()));

        response.set_mut("Posted!");
        Ok(response)
    }

    fn handle_collection_put(&self, req: &mut Request) -> IronResult<Response> {
        let mut body_buf = String::new();
        if let Err(_) = req.body.read_to_string(&mut body_buf) {
            return service_unavailable();
        }

        let new_thread_request = match json::decode(&body_buf) {
            Ok(new_thread_request) => new_thread_request,
            Err(_) => return service_unavailable(),
        };

        let post_result = match self.board.write() {
            Ok(mut write_guard) => write_guard.post_new_thread(new_thread_request),
            Err(_) => return service_unavailable(),
        };

        if let Err(_) = post_result {
            return service_unavailable()
        }

        let mut response = Response::new();
        response.set_mut(status::Ok);
        response.headers.set(ContentType("text/plain".parse().unwrap()));

        response.set_mut("Posted!");
        Ok(response)
    }
}

impl Handler for BoardHandler {
    fn handle(&self, req: &mut Request) -> IronResult<Response> {
        let mut thread_id: Option<PostId> = None;

        if req.url.path.len() > 0 {
            thread_id = req.url.path[0].parse().ok().and_then(|id| Some(PostId(id)));
        }

        match (req.method.clone(), thread_id) {
            (Method::Get, Some(thread_id)) => self.handle_get(thread_id, req),
            (Method::Get, None) => self.handle_collection_get(req),
            (Method::Put, Some(thread_id)) => self.handle_put(thread_id, req),
            (Method::Put, None) => self.handle_collection_put(req),
            _ => Ok(Response::with((status::MethodNotAllowed, "Method not allowed"))),

        }
    }
}


struct ResponseTime;

impl Key for ResponseTime {
    type Value = u64;
}

impl BeforeMiddleware for ResponseTime {
    fn before(&self, req: &mut Request) -> IronResult<()> {
        // Set the current time for retrieval later.
        req.extensions.insert::<ResponseTime>(precise_time_ns());
        Ok(())
    }

    fn catch(&self, req: &mut Request, err: IronError) -> IronResult<()> {
        // On an error just do the same thing.
        let _ = self.before(req);
        Err(err)
    }
}

impl AfterMiddleware for ResponseTime {
    fn after(&self, req: &mut Request, res: Response) -> IronResult<Response> {
        // Get the time we set earlier, compare it to now.
        let delta = precise_time_ns() - *req.extensions.find::<ResponseTime>().unwrap();
        println!("Request took: {} ms", (delta as f64) / 1000000.0);
        Ok(res)
    }

    fn catch(&self, req: &mut Request, err: IronError) -> IronResult<Response> {
        let delta = precise_time_ns() - *req.extensions.find::<ResponseTime>().unwrap();

        // Print something different on errors.
        println!("Request errored, and took: {} ms", (delta as f64) / 1000000.0);
        Err(err)
    }
}

fn main() {
    // let prog = Board::new(BoardInfo::new("prog", "Programming"));
    let mut prog = Mount::new();
    prog.mount("prog", BoardHandler::new(Board::new(BoardInfo::new("prog", "Programming"))));

    let mut root = Mount::new();
    root.mount("boards", prog);

    let mut chain = Chain::new(root);
    chain.link((ResponseTime, ResponseTime));

    Iron::new(chain).http("localhost:3000").unwrap();
}
