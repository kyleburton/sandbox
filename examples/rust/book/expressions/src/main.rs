/*
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
*/

fn addem(x: i32, y: i32) -> i32 {
  x + y
}

fn main () {
  let nums = [(1,2), (2,8), (99, 102)];
  for pair in nums.iter() {
    match pair {
      &(a, b) => println!("addem({}, {}) = {}", a, b, addem(a, b)),
      // NB: the compiler errors on the next line "unreachable pattern"
      // _       => println!("not sure what pair is (and can't print it)")
    }
  }
}
