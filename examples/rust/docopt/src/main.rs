extern crate rustc_serialize;
extern crate docopt;

use std::env;
use docopt::Docopt;

const USAGE: &'static str = "
Example command line argument parsing with docopt.

Usage:
  main (-h | --help)
  main ls
  main ls [-l] <vessel-type>
  main launch <vessel-name>
  main --version

Options:
  -h --help       Show this screen
  -l              Show long listings (full details)
  --version       Show version.
  -v              Enable verbosity
";

#[derive(Debug, RustcDecodable)]
struct Args {
    cmd_ls: bool,
    cmd_launch: bool
}

fn main() {
    for arg in env::args() {
        println!("arg: {}", arg);
    }

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    println!("{:?}", args);
}
