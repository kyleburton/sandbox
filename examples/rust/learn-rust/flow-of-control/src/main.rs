fn main() {

    let n = 5;

    if n < 0 {
        print!("{} is negative", n);
    } else if n > 0 {
        print!("{} is positive", n);
    } else {
        print!("{} is zero", n);
    }

    let big_n =
        if n < 10 && n > -10 {
            println!(", and is a small number, increase ten-fold");
            10 * n
        } else {
            println!(", and is a big number, halve the number");
            n / 2
        };

    println!("n={} -> big_n={}", n, big_n);

    println!("----------------------------------------");

    // loop

    let mut count = 0u32;
    println!("let's count until infinity!");

    loop {
        count += 1;

        if count == 3 {
            println!("three");
            continue;
        }

        println!("count={}", count);

        if count == 5 {
            println!("OK, that's enough");
            break;
        }
    }

    println!("----------------------------------------");

    'outer: loop {
        println!("entered the outer loop");
        #[allow(unused_labels)]
        'inner: loop {
            println!("entered the inner loop");
            break 'outer;
        }
        #[allow(unreachable_code)]
        {
            println!("This point whill never be reached.");
        }
    }
    println!("Exited the outer loop.");

    println!("----------------------------------------");

    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 10 {
            break counter * 2;
        }
    };

    assert_eq!(result, 20);

    println!("----------------------------------------");

    let mut nn = 1;
    while nn < 101 {
        if nn % 15 == 0 {
            println!("nn: fizzbuzz");
        } else if nn % 3 == 0 {
            println!("nn: fizz");
        } else if nn % 5 == 0 {
            println!("nn: buzz");
        } else {
            println!("nn: {}", nn);
        }
        nn += 1;
    }

    println!("----------------------------------------");
    // for loops: https://doc.rust-lang.org/rust-by-example/flow_control/for.html
    for mm in 1..=100 {
        if mm % 15 == 0 {
            println!("mm: fizzbuzz");
        } else if mm % 3 == 0 {
            println!("mm: fizz");
        } else if mm % 5 == 0 {
            println!("mm: buzz");
        } else {
            println!("mm: {}", mm);
        }
    }

    let names = vec!["Bob", "Frank", "Ferris"];
    for name in names.iter() {
        match name {
            &"Ferris" => println!("There is a rusacean among us! ({})", name),
            _ => println!("Hello {}", name),
        }
    }


    // into_iter() "consumes" the vec
    // also, the iterator doesn't result in a ref to a str, just a str
    let names2 = vec!["Bob", "Frank", "Ferris"];
    for name in names2.into_iter() {
        match name {
            "Ferris" => println!("There is a rusacean among us! ({})", name),
            _ => println!("Hello {}", name),
        }
    }


    let mut names3 = vec!["Bob".to_string(), "Frank".to_string(), "Ferris".to_string()];
    for name in names3.iter_mut() {
        *name = match name.as_str() {
            "Ferris" => format!("There is a rusacean among us! ({})", name),
            _ => format!("Hello {}", name),
        }
    }

    println!("names3: {:?}", names3)

}
