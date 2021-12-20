extern crate iron;
extern crate router;

use iron::prelude::*;
use iron::status;
use router::Router;


fn handler (req: &mut Request) -> IronResult<Response> {
    let router = req.extensions.get::<Router>();
    println!("router: {:?}", router);
    println!("GET: url={}", req.url);
    println!("     method={}", req.method);
    println!("     remote_addr={}", req.remote_addr);
    println!("     headers={:?}", req.headers);
    // println!("     body={:?}", req.body);
    let ref query = req.extensions.get::<Router>().unwrap().find("query").unwrap_or("/");
    Ok(Response::with((status::Ok, *query)))
}

fn main() {
    let mut router = Router::new();
    router.get("/",       handler);
    router.get("/:query", handler);

    let address = "localhost:3000";

    println!("On {}", address);
    Iron::new(router).http(address).unwrap();
}
