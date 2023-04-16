fn main() {
    let decimal = 65.4321_f32;

    // no implicit conversion of float to u8
    // let integer: u8 = decimal;

    let integer = decimal as u8;
    let character = integer as char;

    // let character = decimal as char;

    println!("1000 as a u16 is: {}", 1000 as u16);

    // NB: this triggers #[deny(overflowing_literals)]
    // println!("1000 as a u8 is: {}", 1000 as u8);
    println!("-1 as a u8 is: {}", (-1i8) as u8);

    println!("1000 mod 256 is: {}", 1000 % 256);

    println!("128 as a i16 is: {}", 128 as i16);
    // NB: this triggers #[deny(overflowing_literals)]
    // println!("128 as a i8 is: {}", 128 as i8);
    println!("127 as a i8 is: {}", 127 as i8);

    // NB: this triggers #[deny(overflowing_literals)]
    // println!("1000 uas a u8 is: {}", 1000 as u8);
    // println!("232 uas a i8 is: {}", 232 as i8);

    println!("300.0 uas a i8 is: {}", 300.0 as u8);
    println!("-100.0 uas a u8 is: {}", -100.0 as u8);

    unsafe {
        println!("300.0 as u8 is : {}", 300.0_f32.to_int_unchecked::<u8>());

        println!("-100.0 as u8 is : {}", (-100.0_f32).to_int_unchecked::<u8>());
        println!("nan as u8 is : {}", f32::NAN.to_int_unchecked::<u8>());
    }
}
