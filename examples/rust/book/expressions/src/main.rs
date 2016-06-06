fn addem(x: i32, y: i32) -> i32 {
  x + y
}

fn addem2(x: i32, y: i32) -> i32 {
  return x + y
}

fn addem3(x: i32, y: i32) -> i32 {
  return x + y;
}

fn addem4(x: i32, y: i32) -> i32 {
  println!("  NB: addem4({}, {})", x, y);
  x + y
}

fn main () {
  let nums = [(1,2), (2,8), (99, 102)];
  for pair in nums.iter() {
    match pair {
      &(a, b) => {
        println!("addem({}, {})  = {}", a, b, addem(a, b));
        println!("addem2({}, {}) = {}", a, b, addem2(a, b));
        println!("addem3({}, {}) = {}", a, b, addem3(a, b));
        println!("addem4({}, {}) = {}", a, b, addem4(a, b));
        println!("");
      },
      // NB: the compiler errors on a match of _ (any) with "unreachable pattern" 
      // (which is nice)
      // _       => println!("not sure what pair is (and can't print it)")
    }
  }
}
