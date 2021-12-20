extern crate rustc_serialize;
extern crate hyper;

use hyper::client::Client;
use std::time::Duration;
use std::io::Read;
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Person {
    firstname: String,
    lastname:  String,
    message:   Option<String>,
}

fn main() {
    let mut client = Client::new();
    client.set_read_timeout(Some(Duration::new(5, 0)));
    client.set_write_timeout(Some(Duration::new(5, 0)));
    println!("client={:?}", client);

    //let res = client.get("http://[::1]:3000/").send().unwrap();
    let mut res = client.get("http://localhost:3000/").send().unwrap();
    println!("res: {:?}", res);

    let body = &mut String::new();
    let read_result = res.read_to_string(body);
    println!("read_result={:?}", read_result);
    match read_result {
        Ok(size) => println!("  read Ok size={}", size),
        Err(e)   => {
            println!("  read Err e={}", e);
            std::process::exit(1);
        },
    }
    println!("body={}", body);

    let person = Person { 
        firstname: "this".to_string(),
        lastname: "that".to_string(),
        message: None
    };
    let mut res = client.post("http://localhost:3000/people")
        .body(json::encode(&person).unwrap().as_str())
        .send()
        .unwrap();
    assert_eq!(res.status, hyper::Ok);
    let body = &mut String::new();
    let read_result = res.read_to_string(body);
    println!("read_result={:?}", read_result);
    println!("body={}", body);
}
