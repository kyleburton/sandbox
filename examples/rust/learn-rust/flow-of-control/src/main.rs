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
}
