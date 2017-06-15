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
        let next = &padovan[i - 3] + &padovan[i - 2];
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
        let next = padovan[i - 3] + padovan[i - 2];
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
    println!("thinger[{}].chars().count() = {}",
             thinger,
             thinger.chars().count());


    let parts = vec!["this", "that", "other"];
    println!("{}", parts.concat());
    println!("{}", parts.join(", "));


    // let mut s = "frayed knot".to_string();
    let s = "frayed knot".to_string();
    println!("s.len()={}; s.capacity()={}; s='{}'",
             s.len(),
             s.capacity(),
             s);
    // s = s + ", sir";
    // println!("s.len()={}; s.capacity()={}; s='{}'", s.len(), s.capacity(), s);

    // print_padovan();
    // print_padovan_bigint(1000);
    // print_padovan_bigint(100);

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

    // NB: string constants are &str
    // while calling.to_string on an &str produces a std::string::String
    let stuff = vec!["things".to_string(),
                     "stuff".to_string(),
                     "whoper".to_string()];
    println!("stuff is ({})", stuff.join(", "));
    println!("stuff is {:?}", stuff);

    /*
    let mut s = "things and stuffs".to_string();
    let t = s;
    println!("t is {}", t);
    println!("s is {}", s); // this errors b/c t now owns the string
    */

    let mut v = Vec::new();
    for i in 101..106 {
        v.push(i.to_string());
    }
    // let third = v[2];         // NB: can't move an element of a vec
    // let ref third = v[2];     // a ref is ok
    // let ref mut third = v[2]; // ... not sure ...
    // let third = &v[2];        // & makes a ref
    // let third = &mut v[2];    // ... not sure ...
    let third = v[2].clone(); // a clone is ok
    println!("v={:?}", v);
    println!("third={}", third);



    // pg 90

    struct Person {
        name: Option<String>,
        birth: i32,
    };

    let mut composers = Vec::new();
    composers.push(Person {
                       name: Some("Palestrina".to_string()),
                       birth: 1525,
                   });
    // let first_name = composers[0].name; // not ok, can't borrow an elt of the vec
    // since it's an Option we can swap in None
    // let first_name = std::mem::replace(&mut composers[0].name, None);
    // more idiomatic to use Option's take() function
    let first_name = composers[0].name.take();

    println!("pulled out the first_name: {}", first_name.unwrap());
    println!("v[0].name: {:?}", composers[0].name);


    // NB: what's my take-away?  In Rust, every "assignment" is either a move or a copy.
    // it's not a traditional assignment.  Once a value has been sent across an '=',
    // it is then owned by the lvalue -- unless it's a ref.

    // Next: succinctly describe what 'mut' means to Rust ... does
    // Rust also have a 'const'?

    // p91 Ok, a caveat to this is for 'copy' types, types that define
    // a ??? macro?

    // if copying can be expensive, does the rust profiler identify
    // this?  Is there a rust profiler?
    // http://athemathmo.github.io/2016/09/14/tools-for-profiling-rust.html
    // https://llogiq.github.io/2015/07/15/profiling.html
    // https://www.suchin.co/2016/05/11/Introducing-Cargo-Profiler/


    // "An i32 is simply a pattern of bits in memory; it doesn’t own
    // any heap resources ..." -- I suppose that's the razor for
    // values that are (by default) copied vs those that must be moved
    // or borrowed ... so can you borrow an i32?

    let num : i32 = 36;
    let ref num2 : i32 = num;
    let num3 = &num;
    println!("num={}; num2={}; num3={}", num, num2, num3);

    // looks like you can

    // p92, passing values to print moves them!  I suppose that makes
    // sense, though as a newcomer it is a semantic that trips me up.

    // her we go, drive Copy makes a copy for our own structs
    #[derive(Copy, Clone)]
    struct Label { number: u32 }
    fn print(l: Label) {
        println!("STAMP: {}", l.number);
    }

    let l = Label {number: 3};
    print(l);
    // NB: this would be a move, but b/c we've #deriveda copy it's a
    // copy
    println!("My label number is: {}", l.number);
}
