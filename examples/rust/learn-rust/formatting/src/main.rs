// https://doc.rust-lang.org/rust-by-example/hello/print/fmt.html

use std::fmt::{self, Display, Formatter};

struct City {
    name: &'static str,
    lat: f32,
    lon: f32,
}

impl Display for City {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let lat_c = if self.lat >= 0.0 { 'N' } else { 'S' };
        let lon_c = if self.lon >= 0.0 { 'E' } else { 'W' };

        return write!(
            f,
            "{}: {:.3}° {} {:.3}° {}",
            self.name,
            self.lat.abs(),
            lat_c,
            self.lon.abs(),
            lon_c
        );
    }
}

#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
    alpha: u8,
}

impl Display for Color {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        return write!(f, "RBG ({red:3}, {green:3}, {blue:3}, {alpha:3}) 0x{red:02X}{green:02X}{blue:02X}{alpha:02X}",
                      red=self.red,
                      green=self.green,
                      blue=self.blue,
                      alpha=self.alpha);
    }
}

fn main() {
    for city in [
        City {
            name: "Dublin",
            lat: 53.347778,
            lon: -6.259722,
        },
        City {
            name: "Oslo",
            lat: 59.95,
            lon: 10.75,
        },
        City {
            name: "Vancouver",
            lat: 49.25,
            lon: -123.1,
        },
    ]
        .iter()
    {
        println!("{}", *city);
    }
    for color in [
        Color {
            red: 128,
            green: 255,
            blue: 90,
            alpha: 100,
        },
        Color {
            red: 0,
            green: 3,
            blue: 254,
            alpha: 100,
        },
        Color {
            red: 0,
            green: 0,
            blue: 0,
            alpha: 100,
        },
    ]
        .iter()
    {
        // Switch this to use {} once you've added an implementation
        // for fmt::Display.
        println!("{} {:?}", color, *color);
    }
}
