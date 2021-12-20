use time;

pub fn say_hello() {
    println!("Hello, world at {}!", time::now().asctime());
}

pub fn say_goodbye() {
    println!("Goodbye, world at {}!", time::now().asctime());
}
