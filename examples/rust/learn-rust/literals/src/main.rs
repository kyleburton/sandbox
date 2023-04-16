fn main() {
    let x = 1u8;
    let y = 2u32;
    let z = 3f32;

    let i = 1;
    let f = 1.0;

    println!("sizeof `x` in bytes: {}", std::mem::size_of_val(&x));
    println!("sizeof `y` in bytes: {}", std::mem::size_of_val(&y));
    println!("sizeof `z` in bytes: {}", std::mem::size_of_val(&z));
    println!("sizeof `i` in bytes: {}", std::mem::size_of_val(&i));
    println!("sizeof `f` in bytes: {}", std::mem::size_of_val(&f));
}
