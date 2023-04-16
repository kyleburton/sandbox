fn main() {
    let a_binding;

    {
        let x = 0;
        a_binding = x * x;
    }

    println!("a_binding: {}", a_binding);

    let another_binding;

    // println!("another binding: {}", another_binding);

    another_binding = 1;

    println!("another binding: {}", another_binding);
}
