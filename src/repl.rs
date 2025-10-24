use colored::Colorize;
use std::io;
use std::io::Write;

use crate::core::{
    raw_ast::{Parser, TopLevel},
    report::DisplayReport,
    token::lexer::Lexer,
};

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
            print!("{} ", "→".green().bold());
            io::stdout().flush()?;
        }
        run(&input);
    }
}

fn run<'a>(src: &'a str) {
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer);
    let (res, diagnostics) = parser.repl_parse();
    println!("Raw ASTs:");
    match res {
        TopLevel::Stmts(stmts) => {
            for stmt in stmts {
                println!("{stmt}");
            }
        }
        TopLevel::Expr(expr) => {
            println!("{expr}")
        }
    }
    if diagnostics.has_err() {
        eprintln!("{}", "Parse Error".bold().red());
        eprint!("{}", diagnostics.report(&src))
    }
}
