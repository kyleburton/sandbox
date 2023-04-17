use rand::Rng;
use std::str::FromStr;

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

enum Temperature {
    Celsius(i32),
    Farenheit(i32),
}

fn age() -> u32 {
    // 15
    let mut rng = rand::thread_rng();
    rng.gen::<u32>() % 37
}

fn some_number() -> Option<u32> {
    // Some(42)
    let mut rng = rand::thread_rng();
    Some((rng.gen::<u32>() % 3) + 40)
}

enum Frob {
    Bar,
    Baz,
    Qux(u32),
}

fn get_count_item(s: &str) -> (u64, &str) {
    let mut it = s.split(' ');
    let (Some(count_str), Some(item)) = (it.next(), it.next()) else {
        panic!("Can't segment count item pair: '{s}'");
    };

    let Ok(count) = u64::from_str(count_str) else {
        panic!("Can't parse integer: '{count_str}'");
    };

    (count, item)
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

    println!("----------------------------------------");
    println!("guards");

    // let temperature = Temperature::Celsius(35);
    // let temperature = Temperature::Farenheit(23);
    let temperature = Temperature::Farenheit(102);

    match temperature {
        Temperature::Celsius(t) if t > 30 => println!("{}C is above 30 Celcius", t),
        Temperature::Celsius(t)  => println!("{}C is at or below 30 Celcius", t),

        Temperature::Farenheit(t) if t > 86 => println!("{}F is above 86 Farenheit", t),
        Temperature::Farenheit(t)  => println!("{}F is at or below 86 Farenheit", t),
    }

    let number: u8 = 4;
    match number {
        ii if ii == 0 => println!("Zero!"),
        ii if ii > 0 => println!("Greater than zero"),
        _ => unreachable!("Should never get here"),
    }

    println!("----------------------------------------");
    println!("binding");

    println!("tell me what type of person you are");
    match age() {
        0 => println!("I haven't celebrated my first birthday yet"),
        n @ 1 ..= 12 => println!("I'm a child of age {:?}", n),
        n @ 13 ..= 19 => println!("I'm a teen of age {:?}", n),
        n => println!("I'm an old person of age {:?}", n),
    }

    match some_number() {
        Some(nn @ 42) => println!("The answer: {}", nn),
        Some(nn) => println!("Not interesting ...: {}", nn),
        _ => (),
    }

    let optional = Some(7);
    match optional {
        Some(i) => {
            println!("This is a really long string and `{:?}`", i);
        },
        _ => {}
    }

    let number = Some(7);
    let _letter: Option<i32> = None;
    let emoticon: Option<i32> = None;

    if let Some(i) = number {
        println!("matched {:?}", i);
    } else {
        println!("didn't match a number, let's go with a letter");
    }

    let i_like_letters = false;
    if let Some(i) = emoticon {
        println!("Matched: {:?}", i);
    } else if i_like_letters {
        println!("Didn't match a number.  Let's go with a letter!");
    } else {
        println!("I don't like letters, let's go with an emoticon :)");
    }

    let a = Frob::Bar;
    let b = Frob::Baz;
    // let c = Frob::Qux(160);
    let c = Frob::Qux(100);

    if let Frob::Bar = a {
        println!("a is a foobar");
    }

    if let Frob::Bar = b {
        println!("b is a foobar");
    }

    if let Frob::Qux(value) = c {
        println!("c is {}", value);
    }

    if let Frob::Qux(_value @ 100) = c {
        println!("c is one hundred");
    }

    assert_eq!(get_count_item("3 chairs"), (3, "chairs"));


    let mut optional = Some(0);

    loop {
        match optional {
            Some(i) => {
                if i > 9 {
                    println!("Greater than 9, quit!");
                    optional = None;
                } else {
                    println!("`i` is `{:?}`. Try again.", i);
                    optional = Some(i+1);
                }
            },
            _ => { break; }
        }
    }

    let mut optional = Some(0);

    while let Some(ii) = optional {
        if ii > 9 {
            println!("Greater than 9, quit!");
            optional = None;
        } else {
            println!("`ii` is `{:?}`. Try again.", ii);
            optional = Some(ii+1);
        }
    }
}
