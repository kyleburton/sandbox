fn main() {
    let long_lived_binding = 1;

    {
        let short_lived_binding = 2;
        println!("inner short: {}", short_lived_binding);
    }

    // println!("outer short: {}", short_lived_binding);
    println!("outer long: {}", long_lived_binding);


    let shadowed_binding = 1;

    {
        println!("before being shadowed: {}", shadowed_binding);

        let shadowed_binding = "abc";

        println!("after being shadowed: {}", shadowed_binding);
    }

    println!("outside block: {}", shadowed_binding);

    let shadowed_binding = 2;
    println!("shadowed in outside block: {}", shadowed_binding);
}
