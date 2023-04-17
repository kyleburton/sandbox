fn main() {

    let x = 5u32;
    let y = {
        let x_squared = x * x;
        let x_cube = x_squared * x;

        x_cube + x_squared + x
    };

    let z = {
        // this triggers a warnings and returns unit `()`
        x * x;
    };

    let z2 = {
        // ending with a semi-colon triggers a warning
        // and causes "unit" `()` to be returned
        // NB: explicit returns are not supported, eg:
        // return x * x;
        // x * x;
        x * x
    };

    println!("x  is {:?}", x);
    println!("y  is {:?}", y);
    println!("z  is {:?}", z);
    println!("z2 is {:?}", z2);
}
