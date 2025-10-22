use colored::Colorize;
use std::io;
use std::io::Write;

use crate::core::syntax::{ParseErrorContext, Parser};

pub fn repl() -> io::Result<()> {
    loop {
        // Show prompt
        print!("{} ", "Σ".green().bold());
        io::stdout().flush()?;

        // Read input
        let mut input = String::new();
        loop {
            let mut line = String::new();
            io::stdin().read_line(&mut line)?;
            if line.ends_with("\\") {
                input += &line[..line.len() - 1];
            } else if line.ends_with("\\\n") {
                // Unix style
                input += &line[..line.len() - 2];
                input += "\n";
            } else if line.ends_with("\\\r\n") {
                // Windows style
                input += &line[..line.len() - 3];
                input += "\n";
            } else {
                input += &line;
                break;
            }
        }
        run(&input);
    }
}

fn run<'a>(src: &'a str) {
    let parser = Parser::new(src);
    let (syntax, errs) = parser.parse();
    println!("Raw AST:\n{}", syntax);
    if !errs.is_empty() {
        eprintln!("{}", "Parse Error".bold().red());
        eprint!("{}", ParseErrorContext::new(errs, src))
    }
}
