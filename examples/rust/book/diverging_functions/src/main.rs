// NB: the ! is read as 'diverges'
fn diverges () -> ! {
    panic!("this function never returns");
}

fn main () {
    let x :i32 = diverges();
    let s :String = diverges();

    println!("x:{}", x);
    println!("s:{}", s);

    diverges();
}
