#[allow(dead_code)]
fn foo() -> ! {
    panic!("this call never returns");
}

fn some_fn() {
    ()
}


/**

#![feature(never_type)]

fn main () {
  let x: ! = panic!("this call never returns");
  println!("you will never see this line");
}

*/

fn main() {
    let a: () = some_fn();

    println!("The funtion returns and you can see this line: {:?}", a);

    // panics
    // foo();

    fn summ_odd_numbers(up_to: u32) -> u32 {
        let mut acc = 0;
        for ii in 0..=up_to {
            let addition: u32 = match ii%2 == 1 {
                true => ii,
                false => continue,
            };
            acc += addition;
        }
        acc
    }

    println!("Sum of odd numbers up to 9 (including): {}", summ_odd_numbers(9));
}
