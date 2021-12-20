// http://doc.rust-lang.org/std/fmt/
//
// http://rustbyexample.com/hello/print/print_display/testcase_list.html

use std::fmt;

struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let List(ref vec) = *self;

        try!(write!(f, "["));
        for (count, v) in vec.iter().enumerate() {
            if count  != 0 {
                try!(write!(f, ", "));
            }
            try!(write!(f, "{}:{}", count, v));
        }

        write!(f, "]")
    }
}

//  http://rustbyexample.com/hello/print/fmt.html 
#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RGB: ({}, {}, {}) 0x{:02X}{:02X}{:02X}",
          self.red, self.green, self.blue,
          self.red, self.green, self.blue
        )
    }
}

use std::io::Write;
fn main() {
    println!("named params: a={a}, b={b}, c={c}",
             c=123,
             b="forty two",
             a="99"
            );
    let pi = 3.141592653589793238462643383279502884197169399375105820;
    println!("precision:  {:.*}", 0, pi);
    println!("precision:  {:.*}", 1, pi);
    println!("precision:  {:.*}", 2, pi);
    println!("precision:  {:.*}", 3, pi);
    println!("precision:  {:}", pi);

    let num = 23456;
    println!("default:   {}", num);
    println!("octal:     {:o}", num);
    println!("octal:     {:#o}", num);
    println!("hex:       {:x}", num);
    println!("hex:       {:#x}", num);
    println!("HEX:       {:X}", num);
    println!("HEX:       {:#X}", num);
    // println!("pointer:   {:p}", num);
    println!("binary:    {:b}", num);
    println!("binary:    {:#b}", num);

    println!("lower exp: {:e}", 4294967296.0);
    println!("upper exp: {:E}", 4294967296.0);

    /*
    let mut w = Vec::new();
    write!(&mut w, "Hello {}!", "world");
    for x in &w {
        println!("  {}", x);
    }
    */

    let v = List(vec![1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096]);
    println!("{}", v);

    for color in [
        Color { red: 128, green: 255, blue: 90 },
        Color { red: 0, green: 3, blue: 254 },
        Color { red: 0, green: 0, blue: 0 },
        ].iter() {
        println!("{}", color);
        println!("{:?}", color);
        println!("");
    }
}
