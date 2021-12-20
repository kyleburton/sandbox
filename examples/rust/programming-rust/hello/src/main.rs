use std::str::FromStr;

pub fn gcd(mut n: u64, mut m: u64) -> u64 {
    assert!(n != 0 && m != 0);
    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t;
        }
        m = m % n;
    }
    n
}

#[test]
fn test_gcd() {
    assert_eq!(gcd(2 * 5 * 11 * 17, 3 * 7 * 13 * 19), 1);

    assert_eq!(gcd(2 * 3 * 5 * 11 * 17, 3 * 7 * 11 * 13 * 19), 3 * 11);

}

fn main() {
    println!("Hello, world!");

    let lines = "this\nthat\nother\nthing\n";
    for line in lines.lines() {
        println!("Line: '{}'", line);
    }

    // https://doc.rust-lang.org/core/result/enum.Result.html#method.map
    let numbers = "1\n2\n3\n4\n";
    for snum in numbers.lines() {
        match snum.parse::<i32>().map(|ii| ii * 2) {
            Ok(nn) => println!("number: {}", nn),
            Err(ee) => println!("error: {}", ee),
        }
    }

    match u64::from_str("23") {
            Ok(nn) => println!("number: {}", nn),
            Err(ee) => println!("error: {}", ee),
    }
}
