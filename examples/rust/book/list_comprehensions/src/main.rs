extern crate rustc_serialize;

use std::collections::HashMap;
use rustc_serialize::json;

fn main () {
  let counts = "ACGT".chars().map(|c| (c, 0_i32)).collect::<HashMap<_, _>>();
  println!("counts: {}", json::encode(&counts).unwrap());

}
