// http://rustbyexample.com/primitives/tuples.html

fn reverse (pair: (i32, bool)) -> (bool, i32) {
    let (integer, boolean) = pair;
    (boolean, integer)
}


/* see: https://users.rust-lang.org/t/how-can-i-print-the-type-of-a-variable/4183/3
 * (but it doesn't seem to do anything that I can make sense of)
fn typeid<T: std::any::Any>(_: &T) {
    println!("{:?}", std::any::TypeId::of::<T>());
}
*/

#[derive(Debug)]
struct Matrix(f32, f32, f32, f32);

fn main() {
    let long_tuple = (1u8, 2u16, 3u32, 4u64,
                      -1i8, -2i16, -3i32, -4i64,
                      0.1f32, 0.2f64,
                      'a', true
                     );

    println!("long_tuple first value: {}", long_tuple.0);
    println!("long_tuple second value: {}", long_tuple.1);
    println!("long_tuple: {:?}", long_tuple);

    let tuple_of_tuples = ((1u8, 2u16, 2u32), (4u64, -1i8), -2i16);
    println!("tuple_of_tuples: {:?}", tuple_of_tuples);



    let pair = (1, true);
    println!("pair is {:?}", pair);

    println!("the reversed pair is {:?}", reverse(pair));

    // To create one element tuples, the comma is required to tell them apart
    // from a literal surrounded by parentheses
    println!("one element tuple: {:?}", (5u32,));
    println!("just an integer: {:?}", (5u32));
    /*
    print!("one element tuple's type: ");
    typeid(&(5u32,));
    typeid(&Matrix);
    typeid(&(Matrix,));
    */

    //tuples can be destructured to create bindings
    let tuple = (1, "hello", 4.5, true);

    let (a, b, c, d) = tuple;
    println!("{:?}, {:?}, {:?}, {:?}", a, b, c, d);

    let matrix = Matrix(1.1, 1.2, 2.1, 2.2);
    println!("{:?}", matrix)
}
