use crate::repl::repl;
use std::process::exit;

mod core;
mod repl;

fn main() {
    if let Err(e) = repl() {
        eprintln!("IO Error: {e}");
        exit(1)
    }
    print!("Bye!")
}
