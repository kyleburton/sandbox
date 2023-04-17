#[allow(dead_code)]
enum  Color {
    Red,
    Blue,
    Green,
    RGB(u32, u32, u32),
    HSV(u32, u32, u32),
    HSL(u32, u32, u32),
    CMY(u32, u32, u32),
    CMYK(u32, u32, u32, u32),
}

fn main() {
    let number = 13;

    println!("Tell me about {}", number);

    match number {
        1 => println!("One!"),
        2 | 3 | 5 | 7 | 11 => println!("This is a [small] prime"),
        13..=19 => println!("A teen"),
        _ => println!("Nothing speical..."),
    }

    let boolean = true;

    let binary = match boolean {
        false => 0,
        true => 1,
    };

    println!("{} -> {}", boolean, binary);

    println!("----------------------------------------");
    println!("destructuring");
    let triple = (0, -2, 3);

    println!("Tell me about {:?}", triple);
    match triple {
        (0, y, z) => println!("First is `0`, `y` is {:?}, and `z` is {:?}", y, z),
        (1, ..) => println!("First is `1`, rest is 'any'"),
        (.., 2) => println!("Last is `2`, rest is 'any'"),
        (3, .., 4) => println!("First is `3`, last is `4`, and the rest doesn't matter"),
        _ => println!("It doesn't matter what any part is, this is the fall through case"),
    }


    println!("----------------------------------------");
    println!("arrays/slices");

    // let array = [1, -2, 6];
    let array = [4, -2, 6];

    match array {
        [0, second, third] => println!("array[0]=0, array[1]={}, array[2]={}", second, third),
        [1, _, third] => println!("array[0]=1, array[2]={}", third),
        [3, second, tail @ ..] => println!("array[0]=3, array[1]={}, tail={:?}", second, tail),
        [first, middle @ .., last] => println!("array[0]={}, middle={:?}, array[2]={}", first, middle, last),
    }


    println!("----------------------------------------");
    println!("enums");
    println!("----------------------------------------");
    println!("enums");

    // let color = Color::RGB(122, 17, 40);
    let color = Color::CMYK(122, 17, 40, 22);

    println!("what color is it?");

    match color {
        Color::Red => println!("the color is red"),
        Color::Blue => println!("the color is blue"),
        Color::Green => println!("the color is green"),
        Color::RGB(rr, gg, bb) => println!("RGB: red={}, green={}, blue={}", rr, gg, bb),
        Color::HSV(hh, ss, vv) => println!("HSV: hue={}, saturation={}, value={}", hh, ss, vv),
        Color::HSL(hh, ss, ll) => println!("HSV: hue={}, saturation={}, lightness={}", hh, ss, ll),
        Color::CMY(cc, mm, yy) => println!("CMY: cyan={}, magenda={}, yellow={}", cc, mm, yy),
        Color::CMYK(cc, mm, yy, kk) => println!("CMYK: cyan={}, magenda={}, yellow={}, black={}", cc, mm, yy, kk),
    }



    println!("----------------------------------------");
    println!("pointers/ref");

    let reference = &4;
    match reference {
        &val => println!("got a ref via destructuring: {:?}", val),
    }

    match *reference {
        val => println!("got a val destructuring: {:?}", val),
    }

    let _not_a_reference = 3;
    let ref _is_a_reference = 3;
    let value = 5;
    let mut mut_value = 6;
    match value {
        ref rr => println!("got a reference to a value: {:?}", rr),
    }

    match mut_value {
        ref mut mm => {
            *mm += 10;
            println!("[in match] we added 10. `mut_value`={:?}", mm)
        },
    }
    println!("[past match] we added 10. `mut_value`={:?}", mut_value);


    println!("----------------------------------------");
    println!("structs");

    struct Foo {
        x: (u32, u32),
        y: u32,
    }

    // let foo = Foo { x: (1, 2), y: 3 };
    // let foo = Foo { x: (3, 2), y: 3 };
    let foo = Foo { x: (3, 9), y: 7 };
    match foo {
        Foo { x: (1, b), y } => println!("First of x is 1, b={}, y={}", y, b),
        Foo { y: 2, x: i} => println!("y is 2, i={:?}", i),
        Foo { y, .. } => println!("y={}, we don't care about x", y),
    }
}
