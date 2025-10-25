use std::process::exit;

use crate::repl::Repl;

mod core;
mod repl;

fn main() {
    let repl = Repl;
    if let Err(e) = repl.run() {
        eprintln!("IO Error: {e}");
        exit(1)
    }
    print!("Bye!")
}
