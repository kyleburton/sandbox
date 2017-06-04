extern crate num_bigint;
extern crate num_traits;

use num_bigint::BigInt;
use num_traits::One;

fn print_padovan_bigint(limit: usize) {
    let mut padovan = Vec::<BigInt>::new();
    padovan.push(One::one());
    padovan.push(One::one());
    padovan.push(One::one());
    for i in 3..limit {
        let next = &padovan[i-3] + &padovan[i-2];
        padovan.push(next);
    }
    println!("P(1..1000)");
    for pp in padovan {
        println!("{} ", pp);
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
        println!("{} ", pp);
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


    // let mut s = "frayed knot".to_string();
    let s = "frayed knot".to_string();
    println!("s.len()={}; s.capacity()={}; s='{}'", s.len(), s.capacity(), s);
    // s = s + ", sir";
    // println!("s.len()={}; s.capacity()={}; s='{}'", s.len(), s.capacity(), s);

    print_padovan();
    // print_padovan_bigint(1000);
    print_padovan_bigint(100);

    let s1 = vec!["udon", "ramen", "soba"];
    let s: Vec<String> = s1.iter().map(|x| x.to_string()).collect();
    println!("mapped: s={:?}", s);

    {
        // Programming Rust pg 79, Box, pointer/ref to heap allocated data
        let point = Box::new((0.625, 0.5));
        let p2 = (0.625, 0.5);
        let label = format!("{:?}", point);
        let l2 = format!("{:?}", p2);
        assert_eq!(label, "(0.625, 0.5)");
        println!("label={:?}", label);
        println!("l2={:?}", l2);
        // nb: b/c point leaves scope here, it gets freed
    }

    {
        struct Person {
            name: String,
            birth: i32,
        }

        let mut composers = Vec::new();

        composers.push(Person {
            name: "Palestrina".to_string(),
            birth: 1525,
        });
        composers.push(Person {
            name: "Dowland".to_string(),
            birth: 1563,
        });
        composers.push(Person {
            name: "Lully".to_string(),
            birth: 1632,
        });

        for composer in &composers {
            println!("{}, born: {}", composer.name, composer.birth);
        }

    }
}
