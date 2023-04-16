use std::fmt;

struct Circle {
    radius: i32,
}

impl fmt::Display for Circle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Circle{{radius={}}}", self.radius)
    }
}

fn main() {
    let circle = Circle { radius: 6 };
    println!("circle={}", circle.to_string());


    let parsed: i32 = "5".parse().unwrap();
    let turbo_parsed = "10".parse::<i32>().unwrap();

    let sum = parsed + turbo_parsed;

    println!("Sum: {:?}", sum);
}
