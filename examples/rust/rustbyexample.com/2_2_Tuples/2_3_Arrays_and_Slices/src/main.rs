// http://rustbyexample.com/primitives/array.html
use std::mem;

// NB: when using [] to declare arrays, the size is known at compile time (not dynamic)


fn analyze_slice(slice: &[i32]) {
    println!("first element of the slice: {}", slice[0]);
    println!("the slice has {} elements", slice.len());
}

fn print_array(items: &[i32]) {
    for item in items {
        print!("{:?} ", item)
    }
    println!("");
}

fn main() {
    let xs: [i32; 5] = [1, 2, 3, 4, 5];
    // NB: the type signature isn't required
    // let xs2 = [1, 2, 3, 4, 5];

    // All elements can be initialized to the same value
    let ys: [i32; 500] = [0; 500];
    let ys2: [i32; 500] = [1; 500];

    // println!("ys={:?}", ys);
    print_array(&ys);
    print_array(&ys2);

    // Indexing starts at 0
    println!("first element of the array: {}", xs[0]);
    println!("second element of the array: {}", xs[1]);


    // `len` returns the size of the array
    println!("array size: {}", xs.len());

    // Arrays are stack allocated
    println!("array occupies {} bytes", mem::size_of_val(&xs));

    // Arrays can be automatically borrowed as slices
    println!("borrow the whole array as a slice");
    analyze_slice(&xs);

    // Slices can point to a section of an array
    println!("borrow a section of the array as a slice");
    analyze_slice(&ys[1 .. 4]);

    // Out of bound indexing yields a panic
    // println!("{}", xs[5]);
}


