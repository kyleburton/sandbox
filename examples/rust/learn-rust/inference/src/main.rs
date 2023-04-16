fn main() {
    let elem = 5u8;
    let mut vec = Vec::new();
    vec.push(elem);
    println!("vec={:?}", vec);

    // explicitly set the type
    let mut vec2 = Vec::<u16>::new();
    vec2.push(elem as u16);
    println!("vec2={:?}", vec2);

}
