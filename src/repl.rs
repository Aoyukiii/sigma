use colored::Colorize;
use std::io;
use std::io::Write;

use crate::core::lexer::Lexer;
use crate::core::syntax::Parser;

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
    // let mut lexer = Lexer::new(src);
    // loop {
    //     let tok = lexer.next();
    //     if tok.is_eof() {
    //         break;
    //     }
    //     println!("{}", tok);
    // }
    let parser = Parser::new(src);
    let (syntax, errs) = parser.parse();
    println!("{}", syntax);
    for (i, err) in errs.iter().enumerate() {
        println!("[{}] {:?}", i + 1, err)
    }
}
