use std::fmt;

struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vec = &self.0;

        write!(f, "[")?;
        // write!(f, "[");

        for (ii, vv) in vec.iter().enumerate() {
            if ii != 0 {
                write!(f, ",")?;
            }
            write!(f, "{}:{}", ii, vv)?;
        }

        return write!(f, "]");
    }
}

fn main() {
    let v = List(vec![1, 2, 3]);
    println!("v={}", v);
}
