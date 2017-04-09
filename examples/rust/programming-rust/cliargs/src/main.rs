/*
 *  cargo run 4 2 12  three
 * cargo run 4 2 12
 */


use std::io::Write;
use std::str::FromStr;

extern crate hellolib;
use hellolib::gcd;

fn main() {
    let mut numbers = Vec::new();

    println!("main: args().first()={}", std::env::args().nth(0).unwrap());

    for arg in std::env::args().skip(1) {
        numbers.push(u64::from_str(&arg).expect(&*format!("Error parsing argument! '{}'", &arg)));
        // numbers.push(u64::from_str(&arg).unwrap());

        // NB: looks confusing to me, the *format!(...) triggers the
        // Deref of the returned String - returning a str, while the &
        // takes a ref to the str, satisfying the art type to the
        // .expect(...) call
    }

    if numbers.len() == 0 {
        writeln!(std::io::stderr(), "Usage: gcd NUMBER ...").unwrap();
        std::process::exit(1);
    }

    let mut d = numbers[0];
    for m in &numbers[1..] {
        d = gcd(d, *m);
    }

    println!("The greatest common divisor of {:?} is {}", numbers, d);
}
