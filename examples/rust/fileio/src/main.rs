use std::env;
use std::fs;

fn main() {
    let pwd = env::var("PWD").unwrap();
    println!("pwd={:#?}", pwd);
    let contents = fs::read_to_string("main.rs").unwrap();
    println!("contents={:#?}", contents);

    let lines = contents.split("\n");
    // println!("lines={:#?}", lines);

    let mut linesv: Vec<&str> = lines.collect();

    // let lines: Vec<&std::string::String> = contents.split(&"\n".to_string()).collect::<&std::string::String>();
    linesv.reverse();
    for line in &linesv {
        println!("line={}", line);
    }

}
