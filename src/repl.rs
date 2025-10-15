use colored::Colorize;
use std::io;
use std::io::Write;

use crate::core::lexer::lexer;

pub fn repl() {
    loop {
        // Show prompt
        print!("{} ", "Σ".green().bold());
        io::stdout().flush().unwrap();

        // Read input
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim_end(); // Remove trailing newline

        run(input);
    }
}

fn run<'a>(src: &'a str) {
    let tok_stream = lexer(src);
    for tok in tok_stream {
        println!("{}", tok)
    }
}
