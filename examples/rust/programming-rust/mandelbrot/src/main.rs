extern crate num;
use num::Complex;
use std::str::FromStr;

#[allow(dead_code)]
fn escapes(c: Complex<f64>, limit: u32) -> Option<u32> {
    let mut zz = Complex { re: 0.0, im: 0.0 };
    for ii in 0..limit {
        zz = zz * zz + c;
        if zz.norm_sqr() > 4.0 {
            return Some(ii);
        }
    }
    return None;
}

fn parse_pair<T: FromStr>(ss: &str, separator: char) -> Option<(T, T)> {
    match ss.find(separator) {
        None => None,
        Some(index) => {
            match (T::from_str(&ss[..index]), T::from_str(&ss[index + 1..])) {
                (Ok(l), Ok(r)) => Some((l, r)),
                _ => None,
            }
        }
    }
}

#[test]
fn test_parse_pair() {
    assert_eq!(parse_pair::<i32>("", ','), None);
    assert_eq!(parse_pair::<i32>("10,", ','), None);
    assert_eq!(parse_pair::<i32>(",10", ','), None);
    assert_eq!(parse_pair::<i32>("10,20", ','), Some((10, 20)));
    assert_eq!(parse_pair::<i32>("10,20xy", ','), None);
    assert_eq!(parse_pair::<f64>("0.5x", 'x'), None);
    assert_eq!(parse_pair::<f64>("0.5x1.5", 'x'), Some((0.5, 1.5)));

}

fn main() {
    println!("Hello, world!");
}
