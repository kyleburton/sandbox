fn main() {
    let x = 32;
    println!("Hello, world2! x={}", x);
    // NB: it looks like when you shadow, you can change a variable's type
    let x = "three";
    println!("Hello, world2! x={}", x);

    let mut x = 32;
    println!("Hello, world2! x={}", x);
    x = 16;
    println!("Hello, world2! x={}", x);
    // NB: x is now immutable!
    let x = x + -8;
    println!("Hello, world2! x={}", x);
}
