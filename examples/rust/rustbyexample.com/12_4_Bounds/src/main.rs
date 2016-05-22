// use std::fmt::Debug;

#[derive(Debug)]
struct Rectangle {
    x: i32,
    y: i32
}

trait HasArea {
    fn area(&self) -> f64;
}

impl HasArea for Rectangle {
    fn area(&self) -> f64 {
        (self.x * self.y) as f64
    }
}

fn area<T: HasArea>(t: &T) -> f64 {
    t.area()
}

fn main() {
    let r = Rectangle {
        x: 5,
        y: 5
    };
    println!("r={:?}", r);
    println!("r.area:    {}", r.area());
    println!("area(r):   {}", area(&r));
}
