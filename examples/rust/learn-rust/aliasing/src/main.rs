type NanoSecond = u64;
type Inch = u64;
type U64 = u64;

fn main() {
    let nanoseconds: NanoSecond = 5 as U64;
    let inches: Inch = 2 as U64;
    println!("{} nanoseconds + {} inches = {} though the unit doesn't make sense does it?",
             nanoseconds,
             inches,
             nanoseconds + inches);
}
