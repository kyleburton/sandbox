// http://rustbyexample.com/hello/print/print_display.html
use std::fmt;


struct Structure(i32);

impl fmt::Display for Structure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Structure{{{}}}", self.0)
    }
}

fn main() {
    let st = Structure(42);
    println!("st is: '{}'", st);
}
