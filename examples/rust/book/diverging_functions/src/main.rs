fn diverges () -> ! {
    panic!("this function never returns");
}

fn main () {
    diverges();
}
