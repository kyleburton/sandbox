fn main() {
    let x = 5 + 5;
    println!("Is `x` 10 or 100? x={x}", x=x);
    println!("x:b={x:b}", x=x);
    println!("x:b={x:b}", x=x);
    println!("x:o={x:x}", x=x);
    println!("x:x={x:x}", x=x);
    println!("x:X={x:X}", x=x);


    println!("x:0>5={x:0>5}", x=x);
    println!("x:0<5={x:0<5}", x=x);

    println!("x:#08b={x:#08b}", x=x);
    println!("x:#010b={x:#010b}", x=x);


    println!("x:0>width$={x:0>width$}", x=x, width=5);
    println!("x:0<width$={x:0<width$}", x=x, width=5);

    #[allow(dead_code)]
    struct Structure(i32);
    // println!("This struct `{}` wont' print...", Structure(3));

    let number: f64 = 1.0;
    let width: usize = 5;

    println!("number={number}; width={width} :: {{number:>width$}}={number:>width$}");

    let pi = 3.141592;
    println!("Pi is roungly {pi:.3}", pi=pi);
}
