// http://zsiciarz.github.io/24daysofrust/book/day2.html
extern crate primal;

use primal::Sieve;

// http://stackoverflow.com/questions/21747136/how-do-i-print-the-type-of-a-variable-in-rust

// # ! [feature(core_intrinsics)]
// fn print_type_of<T>(_: &T) -> () {
//     let type_name =
//         unsafe {
//             std::intrinsics::type_name::<T>()
//         };
//     println!("{}", type_name);
// }

fn num_divisors(n: usize, primes: &Sieve) -> Option<usize> {
    match primes.factor(n) {
        Ok(factors) => Some(factors.into_iter().fold(1, |acc, (_, x)| acc * (x + 1))),
        Err(_) => None,
    }
}

fn main() {
    let sieve = Sieve::new(10000);

    let suspect = 5273;
    println!("{} is prime: {}", suspect, sieve.is_prime(suspect)); // true

    let not_a_prime = 1024;
    println!("{} is prime: {}", not_a_prime, sieve.is_prime(not_a_prime)); // guess

    let n = 1000;
    match sieve.primes_from(0).nth(n - 1) {
        Some(number) => println!("{}th prime is {}", n, number),
        None => println!("I don't know anything about {}th prime.", n),
    }


    println!("factor(2610): {:?}", sieve.factor(2610));

    println!("num_divisors(2610): {:?}", num_divisors(2610, &sieve));
}
