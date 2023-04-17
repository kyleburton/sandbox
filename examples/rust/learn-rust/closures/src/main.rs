// https://doc.rust-lang.org/rust-by-example/fn/closures/capture.html
fn apply<F>(f: F) where
    F: FnOnce() {
    // F: Fn() {
    // F: FnMut() {

    f();
}

fn apply_to_3<F>(f: F) -> i32 where
    F: Fn(i32) -> i32 {

    f(3)
}


fn call_me<F: Fn()>(f: F) {
    f();
}

fn function () {
    println!("I'm a function");
}

fn create_fn() -> impl Fn() {
    let text = "Fn".to_owned();
    move || println!("This is a: {}", text)
}

fn create_fnmut() -> impl FnMut() {
    let text = "FnMut".to_owned();
    move || println!("This is a: {}", text)
}

fn create_fnonce() -> impl FnOnce() {
    let text = "FnOnce".to_owned();
    move || println!("This is a: {}", text)
}

fn main() {
    use std::mem;

    let outer_var = 42;

    let closure_annotated = |ii: i32| -> i32 { ii + outer_var };
    let closure_inferred  = |ii| ii + outer_var ;
    println!("closure_annotated: {}", closure_annotated(1));
    println!("closure_inferred: {}", closure_inferred(1));

    let one = || 1;
    println!("closure returning 1: {}", one());

    println!("----------------------------------------");
    println!("capturing");

    // use std::mem;
    let color = String::from("green");
    // are to_string and String::from the same thing?
    assert_eq!("green".to_string(), String::from("green"));

    let print = || println!("`color`:{}", color);

    print();
    let _reborrow = &color;
    print();

    let _color_moved = color;
    // can't use it again:
    // print();

    let mut count = 0;
    let mut inc = || {
        count += 1;
        println!("`count`: {}", count);
    };

    inc();
    // let _reborrow = &count;
    inc();

    let movable = Box::new(3);
    let consume = || {
        println!("`movable`: {:?}", movable);
        mem::drop(movable);
    };

    consume();

    // consume();

    let haystack = vec![1, 2, 3];

    let contains = move |needle| haystack.contains(needle);
    // let contains = |needle| haystack.contains(needle);

    println!("contains 1? {}", contains(&1));
    println!("contains 4? {}", contains(&4));

    // need to remove the `move` from the contains closure to use it in this scope
    // println!("there are {} elemetns in vec", haystack.len());


    // https://doc.rust-lang.org/rust-by-example/fn/closures/input_parameters.html
    println!("----------------------------------------");
    println!("as input parameters");

    let greeting = "hello";
    let mut farewell = "goodbye".to_owned();

    let diary = || {
        println!("I said {}.", greeting);

        farewell.push_str("!!!");
        println!("Then I screamed {}.", farewell);
        println!("Now I can sleep. zzzzz");
        mem::drop(farewell);
    };

    apply(diary);

    let double = |x| 2 * x;
    println!("3 doulbed: {}", apply_to_3(double));

    // https://doc.rust-lang.org/rust-by-example/fn/closures/anonymity.html
    println!("----------------------------------------");
    println!("type anonymity");

    let x = 7;
    let print = || println!("x={}", x);

    apply(print);


    // https://doc.rust-lang.org/rust-by-example/fn/closures/input_functions.html
    println!("----------------------------------------");
    println!("input functions");

    let closure = || println!("I'm a closure");
    call_me(closure);
    call_me(function);


    // https://doc.rust-lang.org/rust-by-example/fn/closures/output_parameters.html
    println!("----------------------------------------");
    println!("as output parameters");
    let fn_plain = create_fn();
    let mut fn_mut = create_fnmut();
    let fn_once = create_fnonce();

    fn_plain();
    fn_mut();
    fn_once();

    // https://doc.rust-lang.org/rust-by-example/fn/closures/closure_examples.html
    println!("----------------------------------------");
    println!("examples in `std`");

    let vec1 = vec![1, 2, 3];
    let vec2 = vec![4, 5, 6];

    println!("2 in vec1: {}", vec1.iter()     .any(|&x| x == 2));
    // NB: into_iter moves the vec and it cannot be used again
    println!("2 in vec2: {}", vec2.into_iter().any(|x| x == 2));

    // any borrows, we can continue to use vec1 and vec2
    println!("vec1 len: {}", vec1.len());
    println!("first element of vec1: {}", vec1[0]);

    let array1 = [1, 2, 3];
    let array2 = [4, 5, 6];

    println!("2 in array1: {}", array1.iter()      .any(|&x| x == 2));
    println!("2 in array2: {}", array2.into_iter().any(|x| x == 2));

    ////////////////////////////////////////
    let vec1 = vec![1, 2, 3];
    let vec2 = vec![4, 5, 6];

    let mut iter = vec1.iter();
    let mut into_iter = vec2.into_iter();

    println!("Find 2 in vec1: {:?}", iter     .find(|&&x| x == 2));
    println!("Find 2 in vec2: {:?}", into_iter.find(| &x| x == 2));

    let array1 = [1, 2, 3];
    let array2 = [4, 5, 6];

    println!("Find 2 in array1: {:?}", array1.iter()     .find(|&&x| x == 2));
    println!("Find 2 in array2: {:?}", array2.into_iter().find(| &x| x == 2));

    let vec = vec![1, 9, 3, 3, 13, 2];

    let index_of_first_even_number = vec.iter().position(|&x| x % 2 == 0);
    assert_eq!(index_of_first_even_number, Some(5));
    println!("index_of_first_even_number: {:?}", index_of_first_even_number);
    if let Some(idx) = index_of_first_even_number {
        println!(" idx={idx}");
    }

    let index_of_first_negative_number = vec.into_iter().position(|x| x < 0);
    assert_eq!(index_of_first_negative_number, None);
    println!("index_of_first_even_number: {:?}", index_of_first_negative_number);

    println!("last index_of_first_even_number: {:?}", vec1.iter().rposition(|&x| x % 2 == 0).unwrap());
    // this one blows up
    // println!("last index_of_first_negative_number: {:?}", vec1.into_iter().rposition(|x| x < 0).unwrap());

    // find_map

    let a = ["lol", "NaN", "2", "5"];
    // str::parse returns a Result<F, <F as FromStr>::Err>, .ok() convers the Result to an Option<F>
    // find_map performs conversion and returns the first Some() (not `None`)
    let first_number: Option<i32> = a.iter().find_map(|s| s.parse().ok());
    println!("first_number: {:?}", first_number);
}
