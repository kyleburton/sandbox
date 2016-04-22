// https://doc.rust-lang.org/rustc-serialize/rustc_serialize/json/index.html
extern crate rustc_serialize;

use rustc_serialize::json;

macro_rules! map(
  { $($key:expr => $value:expr),+ } => {
    {
      let mut m = ::std::collections::HashMap::new();
      $(
        m.insert($key, $value);
      )+
      m
    }
  }
);

#[derive(RustcDecodable, RustcEncodable)]
pub struct Person {
  email: String,
  name:  String,
}

fn main () {
  let names = map!{
    "krburton" => map!{
      "email" => "kyle.burton@gmail.com",
      "name"  => "Kyle Burton"
    },
    "klburton" => map!{
      "email" => "klburton@gmail.com",
      "name"  => "Kristin Burton"
    }
  };

  let kyle = Person {
    email: "kyle.burton@gmail.com".to_string(),
    name:  "Kyle Burton".to_string(),
  };

  // println!("names={}", json::encode(&names))
  println!("true: {}", json::encode(&Some(true)).unwrap());
  println!("false: {}", json::encode(&Some(false)).unwrap());
  println!("names: {}", json::encode(&names).unwrap());
  println!("kyle: {}", json::encode(&kyle).unwrap());
}
