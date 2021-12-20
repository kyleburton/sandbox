* https://techsaju.wordpress.com/2015/09/12/logging-in-rust-using-log/
* http://rust-lang-nursery.github.io/log/env\_logger/


```sh
$ RUST_LOG=trace cargo run
$ RUST_LOG=debug cargo run
$ RUST_LOG=info cargo run
$ RUST_LOG=warn cargo run
$ RUST_LOG=error cargo run
$ RUST_LOG=myns=trace cargo run
# NB: this is documented to work by env\_logger, but does not seem to:
$ RUST_LOG="error,main=error,main::myns=debug" cargo run
```
