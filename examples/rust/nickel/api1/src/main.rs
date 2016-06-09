extern crate rustc_serialize;
#[macro_use] 
extern crate nickel;

use nickel::{Nickel, HttpRouter, JsonBody};
use rustc_serialize::json;

#[derive(RustcDecodable, RustcEncodable)]
struct Person {
    firstname: String,
    lastname:  String,
    message:   Option<String>,
}

fn main() {
    let mut server = Nickel::new();

    /*
    server.utilize(router! {
        get "**" => |_req, _res| {
            "Hello world"
        }
    });
    */

    server.get("/", middleware!("request to root"));
    server.get("/user/:userid", middleware! { |request|
        format!("paramter userid={}", request.param("userid").unwrap())
    });
    server.get("/**", middleware!("catchall"));

    server.post("/people", middleware! { |request, response|
        let person = request.json_as::<Person>().unwrap();
        let person2 = Person {
            firstname: person.firstname.clone(),
            lastname:  person.lastname.clone(),
            message:   Some(format!("Hello {} {}", person.firstname, person.lastname)),
        };

        json::encode(&person2).unwrap()
    });

    let address = "localhost:3000";
    server.listen(address);
}
