// Pg 93 Rc and Arc: shared ownership
// Rc:  reference count -- is _not_ thread safe
// Arc: atomic reference count -- is thread safe
extern crate rand;

use std::rc::Rc;
use std::collections::HashMap;

use rand::Rng;

fn main() {
    let s : Rc<String> = Rc::new("shirataki".to_string());
    let t : Rc<String> = s.clone();
    let u : Rc<String> = s.clone();
    println!("s: {}", s);
    println!("t: {}", t);
    println!("u: {}", u);


    // nb: with Rc<T> and Box<T>, you can use any of the contained types methods directly on the Rc<T>
    // todo: make sure you understand the difference between Box and Rc

    assert!(s.contains("shira"));
    assert_eq!(t.find("taki"), Some(5));
    println!("{} are quite chewy, almost bouncy, but lack flavor", u);

    // "Rust's memory and thread safety guarantees depend on ensuring
    // that no value is every simultaneously shared and mutable."


    // Chapter 5: References pg97
    // non-owning pointer types "references"


    type Table = HashMap<String, Vec<String>>;

    fn show(table: &Table) {
        for (artist, works) in table {
            println!("works by {}:", artist);
            for work in works {
                println!("  {}", work);
            }
        }
    }

    let mut table = Table::new();
    table.insert("Gesualdo".to_string(),
                 vec!["many madrigals".to_string(),
                      "Tenebrae Responsoria".to_string()]
    );
    table.insert("Caravaggio".to_string(),
                 vec!["The Musicians".to_string(),
                      "The Calling of St. Matthew".to_string()]
    );
    table.insert("Cellini".to_string(),
                 vec!["Perseus with the head of Medusa".to_string(),
                      "a salt cellar".to_string()]
    );
    show(&table);
    // nb: when show() took just a Table, ownership was 'moved' to the
    // show fn, so we can't use table b/c it's now uninitialized, when
    // we changed show() to take a ref Table (&Table) we haven't given
    // away ownership

    assert_eq!(table["Gesualdo"][0], "many madrigals");

    fn sort_works(table: &mut Table) {
        for (_artist, works) in table {
            works.sort();
        }
    }

    println!("");
    println!("After sorting");
    println!("");
    sort_works(&mut table);
    show(&table);
        
    // pg 102
    let x = 10;
    let y = 20;
    let mut r = &x;

    let mut rng = rand::thread_rng();
    fn rand_bool(rng : & mut rand::ThreadRng) -> bool {
        let val : u32 = rng.gen();
        return (val % 2) == 1;
        // alternatively:
        // return rng.gen_weighted_bool(2);
    }
    let b = rng.gen();
    for _idx in 1..10 {
        println!("rand_bool(rng): {}", rand_bool(& mut rng));
    }
    println!("b is: {}", b);
    if b {
        r = &y;
    }

    assert!(*r == 10 || *r == 20);
}
