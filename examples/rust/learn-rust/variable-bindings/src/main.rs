fn main() {
    let an_integer = 1u32;
    let a_boolean = true;

    let unit = ();

    let copied_integer = an_integer;

    println!("An integer: {:?}", copied_integer);
    println!("A boolean: {:?}", a_boolean);
    println!("Meet the unit value: {:?}", unit);

    let _unused_variable = 3u32;
    let _noisy_unused_variable = 2u32;
}
