#[allow(dead_code)]
fn sieve_of_primes() {
    const LEN: usize = 100_000;
    // const LEN: usize = 8_187_500; // OK
    // const LEN: usize = 8_250_000; // OK
    // const LEN: usize = 8_300_000; // OK
    // const LEN: usize = 8_350_000; // OK
    // const LEN: usize = 8_360_000; // OK
    // const LEN: usize = 8_362_500; // stack overflow SIGABRT
    // const LEN: usize = 8_365_000; // stack overflow SIGABRT
    // const LEN: usize = 8_370_000; // SIGSEGV
    // const LEN: usize = 8_375_000; // SIGSEGV
    // const LEN: usize = 8_390_000; // SIGSEGV
    // const LEN: usize = 8_400_000; // SIGSEGV
    // const LEN: usize = 8_500_000; // SIGSEGV
    // const LEN: usize = 8_600_000; // SIGSEGV
    // const LEN: usize = 8_500_000; // SIGSEGV
    // const LEN: usize = 9_000_000; // SIGSEGV
    // const LEN: usize = 10_000_000; // NB: this causes a SIGSEGV!
    let mut sieve = [true; LEN];
    let upper: usize = f64::sqrt(LEN as f64) as usize + 1;
    for ii in 2..upper {
        if sieve[ii] {
            let mut jj = ii * ii;
            while jj < LEN {
                sieve[jj] = false;
                jj += ii
            }
        }
    }

    for ii in 2..LEN {
        if sieve[ii] {
            println!("prime: {}", ii);
        }
    }
}

fn main() {
    println!("Hello, world!");

    let t = (12, "eggs");
    let b = Box::new(t); // allocated on the heap...as a copy of t?
    println!("t={:?}", t);
    println!("b={:?}", b);
    // sieve_of_primes();

    // p63 Vectors
    let mut v = vec![2, 3, 5, 7];
    assert_eq!(v.iter().fold(1, |a, b| a * b), 210);
    v.push(11);
    v.push(13);
    println!("folding: {:?}", v);
    assert_eq!(v.iter().fold(2, |a, b| a * b), 60060);

    let mut v2 = Vec::new();
    v2.push("step");
    v2.push("on");
    v2.push("no");
    v2.push("pets");
    assert_eq!(v2, vec!["step", "on", "no", "pets"]);

    let v3: Vec<i64> = (0..5).collect();
    println!("ints: {:?}", v3);
    /*
    let v4: Vec<char> = ('a'..'z').collect();
    println!("chars: {:?}", v4);
    */

    let mut v5 = Vec::<i32>::with_capacity(2);
    assert_eq!(v5.len(), 0);
    assert_eq!(v5.capacity(), 2);
    println!("0: v5={:?}", v5);

    v5.push(1);
    v5.push(2);
    assert_eq!(v5.len(), 2);
    assert_eq!(v5.capacity(), 2);
    println!("2: v5={:?}", v5);

    v5.push(3);
    assert_eq!(v5.len(), 3);
    assert_eq!(v5.capacity(), 4);
    println!("3: v5={:?}", v5);

    v5.insert(2, 35);
    println!("4: v5={:?}", v5);

    // v5.remove(v5.len()-1);  // why doesn't this work?
    // ==> error[E0502]: cannot borrow `v5` as immutable because it is also borrowed as mutable
    v5.remove(3);
    println!("5: v5={:?}", v5);


    assert_eq!(v5.pop(), Some(35));
    assert_eq!(v5.pop(), Some(2));
    assert_eq!(v5.pop(), Some(1));
    assert_eq!(v5.pop(), None);

    // let languages: Vec<String> = std::env::args().skip(1).collect();
    // haha, very funny (and wrong: Haskell OCaml)
    let languages = "prog-name Lisp Scheme C C++ Fortran".split(" ");
    for l in languages {
        println!("{}: {}",
                 l,
                 if l.len() % 2 == 0 {
                     "functional"
                 } else {
                     "imperative"
                 });
    }
}
