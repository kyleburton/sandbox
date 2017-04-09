extern crate num;
extern crate image;
extern crate crossbeam;
use num::Complex;
use std::str::FromStr;
use image::ColorType;
use image::png::PNGEncoder;
use std::fs::File;
use std::io::Write;

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

// TODO: I can I write a generic function that parses a delimited
// string & type converts to a std::Vec?
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

fn pixel_to_point(bounds: (usize, usize),
                  pixel: (usize, usize),
                  upper_left: (f64, f64),
                  lower_right: (f64, f64))
                  -> (f64, f64) {
    let (width, height) = (lower_right.0 - upper_left.0, upper_left.1 - lower_right.1);
    (upper_left.0 + pixel.0 as f64 * width / bounds.0 as f64,
     upper_left.1 - pixel.1 as f64 * height / bounds.1 as f64)
}

#[test]
fn test_pixel_to_point() {
    assert_eq!(pixel_to_point((100, 100), (25, 75),
                              (-1.0, 1.0), (1.0, -1.0)),
    (-0.5, -0.5));
}

fn render(pixels: &mut [u8],
          bounds: (usize, usize),
          upper_left: (f64, f64),
          lower_right: (f64, f64)) {
    assert!(pixels.len() == bounds.0 * bounds.1);
    for r in 0..bounds.1 {
        for c in 0..bounds.0 {
            let point = pixel_to_point(bounds, (c, r), upper_left, lower_right);
            pixels[r * bounds.0 + c] = match escapes(Complex {
                                                         re: point.0,
                                                         im: point.1,
                                                     },
                                                     255) {
                None => 0,
                Some(count) => 255 - count as u8,
            }
        }
    }
}

fn write_bitmap(fname: &str, pixels: &[u8], bounds: (usize, usize)) -> Result<(), std::io::Error> {
    let output = File::create(fname)?;
    let encoder = PNGEncoder::new(output);
    encoder
        .encode(&pixels,
                bounds.0 as u32,
                bounds.1 as u32,
                ColorType::Gray(8))?;
    Ok(())
}

fn main_serial() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 5 {
        writeln!(std::io::stderr(),
                 "Usage: mandelbrot FILE PIXELS UPPERLEFT LOWERRIGHT0")
                .unwrap();
        writeln!(std::io::stderr(),
                 "Eg: {} mandel.png 1000x750 -1.20,0.35 -1,0.20",
                 args[0])
                .unwrap();
        std::process::exit(1);
    }

    let bounds = parse_pair(&args[2], 'x').expect("Error parsing PIXELS!)");
    let upper_left = parse_pair(&args[3], ',').expect("Error parsing UPPER_LEFT!");
    let lower_right = parse_pair(&args[4], ',').expect("Error parsing LOWER_RIGHT!");

    let mut pixels = vec![0; bounds.0 * bounds.1];

    render(&mut pixels, bounds, upper_left, lower_right);

    write_bitmap(&args[1], &pixels, bounds).expect("Error writing PNG file!");
}


//  time ./target/release/mandelbrot mandel.png 6000x4000 -2.0,1.0 1.0,-1.0 && eog mandel.png
fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 5 {
        writeln!(std::io::stderr(),
                 "Usage: mandelbrot FILE PIXELS UPPERLEFT LOWERRIGHT0")
                .unwrap();
        writeln!(std::io::stderr(),
                 "Eg: {} mandel.png 1000x750 -1.20,0.35 -1,0.20",
                 args[0])
                .unwrap();
        std::process::exit(1);
    }

    let bounds = parse_pair(&args[2], 'x').expect("Error parsing PIXELS!)");
    let upper_left = parse_pair(&args[3], ',').expect("Error parsing UPPER_LEFT!");
    let lower_right = parse_pair(&args[4], ',').expect("Error parsing LOWER_RIGHT!");

    let mut pixels = vec![0; bounds.0 * bounds.1];

    let threads = 8;
    let band_rows = bounds.1 / threads + 1;
    {
        let bands: Vec<&mut [u8]> = pixels.chunks_mut(band_rows * bounds.0).collect();
        // crossbeam::scope(|scope| for (i, band) in bands.to_iter().enumerate() {
        crossbeam::scope(|scope| for (i, band) in bands.into_iter().enumerate() {
                             let top = band_rows * i;
                             let height = band.len() / bounds.0;
                             let band_bounds = (bounds.0, height);
                             let band_upper_left =
                                 pixel_to_point(bounds, (0, top), upper_left, lower_right);
                             let band_lower_right = pixel_to_point(bounds,
                                                                   (bounds.0, top + height),
                                                                   upper_left,
                                                                   lower_right);
                             scope.spawn(move || {
                                             render(band,
                                                    band_bounds,
                                                    band_upper_left,
                                                    band_lower_right);
                                         });
                         });
    }
    // render(&mut pixels, bounds, upper_left, lower_right);

    write_bitmap(&args[1], &pixels, bounds).expect("Error writing PNG file!");
}
