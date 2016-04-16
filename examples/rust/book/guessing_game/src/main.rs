// https://doc.rust-lang.org/book/guessing-game.html
extern crate rand;

use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn some_rands() {
  for _ in 0..100 {
    let rnd_number = rand::thread_rng().gen_range(1, 101);
    println!("The random number is: {}", rnd_number);
  }
}

fn main() {
    if false {
      some_rands();
    }

    println!("Guess the number!");

    let cheat = false;

    let secret_number = rand::thread_rng().gen_range(1, 101);

    if cheat {
        println!("The secret number is: {}", secret_number);
    }

    loop {
        println!("Please input your guess.");

        let mut guess: String = String::new();

        io::stdin().read_line(&mut guess)
          .expect("Failed to read line");
    
        println!("You guessed: {}/len={}", guess.trim(), guess.len());
    
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Error: {} is not a number! (at least to me)", guess.trim());
                continue;
            },
        };
    
        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal   => {
              println!("You win!");
              break;
            },
        }
    }
}
