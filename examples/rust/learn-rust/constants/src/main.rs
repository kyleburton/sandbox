static LANGUAGE: &str = "Rust";
const THRESHOLD: i32 = 10;

fn is_big(nn: i32) -> bool {
    nn > THRESHOLD
}

fn main() {
    let nn = 16;

    println!("This is {}", LANGUAGE);
    println!("The threshold is {}", THRESHOLD);
    println!("{} is {}", nn, if is_big(nn) { "big" } else { "small"} );
    // THRESHOLD = 5;
}
