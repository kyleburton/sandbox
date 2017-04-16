extern crate num_bigint;
extern crate num_traits;

use num_bigint::BigInt;
use num_traits::One;

fn print_padovan_bigint() {
    let mut padovan = Vec::<BigInt>::new();
    padovan.push(One::one());
    padovan.push(One::one());
    padovan.push(One::one());
    for i in 3..1000 {
        let next = &padovan[i-3] + &padovan[i-2];
        padovan.push(next);
    }
    println!("P(1..1000)");
    for pp in padovan {
        print!("{} ", pp);
    }
    println!("");
}
    
fn print_padovan() {
    let mut padovan = vec![1i64, 1i64, 1i64];
    for i in 3..100 {
        let next = padovan[i-3] + padovan[i-2];
        padovan.push(next);
    }
    println!("P(1..100)");
    for pp in padovan {
        print!("{} ", pp);
    }
    println!("");
}

fn main() {
    println!("Hello, world!");

    let thinger = "(⊙_◎)";
    println!("thinger[{}].len() = {}", thinger, thinger.len());
    println!("thinger[{}].chars().count() = {}", thinger, thinger.chars().count());


    let parts = vec!["this", "that", "other"];
    println!("{}", parts.concat());
    println!("{}", parts.join(", "));


    let mut s = "frayed knot".to_string();
    println!("s.len()={}; s.capacity()={}; s='{}'", s.len(), s.capacity(), s);
    s += ", sir";
    println!("s.len()={}; s.capacity()={}; s='{}'", s.len(), s.capacity(), s);

    print_padovan();
    print_padovan_bigint();
}
