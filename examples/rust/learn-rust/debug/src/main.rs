use serde::{Serialize, Deserialize};

#[derive(Debug)]
struct Strucutre(i32);

#[derive(Debug)]
struct Deep(Strucutre);

#[derive(Serialize, Deserialize, Debug)]
struct Person<'a> {
    name: &'a str,
    age: u8
}


fn main () {
    println!("{:?} months in a year.", 12);
    println!("{1:?} {0:?} is the {actor:?} name.",
             "Slater", "Christian", actor="actor's");

    println!("Now {:?} will print!", Strucutre(3));
    println!("Now {:?} will print!", Deep(Strucutre(7)));


    let name = "Peter";
    let age = 27;
    let peter = Person { name, age };

    println!("{:#?}", peter);
}
