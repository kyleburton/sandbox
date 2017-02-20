use std::io::Write;
use std::str::FromStr;

extern crate hellolib;
use hellolib::gcd;

fn main() {
    let mut numbers = Vec::new();

    for arg in std::env::args().skip(1) {
        numbers.push(u64::from_str(&arg)
                     .expect("Error parsing argument! '{}'"));
        // how can we pass a more meaningful message to expect? the following doesn't work
        // .expect(stringify!(format!("Error parsing argument! '{}'", arg))));
    }

    if numbers.len() == 0 {
        writeln!(std::io::stderr(), "Usage: gcd NUMBER ...").unwrap();
        std::process::exit(1);
    }

    let mut d = numbers[0];
    for m in &numbers[1..] {
        d = gcd(d, *m);
    }

    println!("The greatest common divisor of {:?} is {}",
             numbers, d);
}
