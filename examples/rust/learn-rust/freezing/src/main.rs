fn main() {
    let mut _mutable_integer = 7i32;

    {
        // shadows _mutable_integer
        let _mutable_integer = _mutable_integer;

        // NB: not mutable in this scope
        // _mutable_integer = 50;
    }
    _mutable_integer = 3;
}
