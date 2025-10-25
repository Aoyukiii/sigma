use colored::Colorize;
use std::io;
use std::io::Write;

use crate::core::{
    raw_ast::{Parser, diagnostics::Diagnostics, stmt::Stmt},
    report::DisplayReport,
    token::lexer::Lexer,
};

pub struct Repl;

impl Repl {
    pub fn run(&self) -> io::Result<()> {
        loop {
            print!("{} ", "Σ".green().bold());
            io::stdout().flush()?;

            let input = self.read_input()?;
            let (res, diagnostics) = self.parse(&input);
            self.show_raw_ast(res);
            self.show_diagnostics(diagnostics, &input);
        }
    }

    fn parse(&self, src: &str) -> (Vec<Stmt>, Diagnostics) {
        let lexer = Lexer::new(src);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }

    fn show_raw_ast(&self, stmts: Vec<Stmt>) {
        println!("Raw ASTs:");
        for stmt in stmts {
            println!("{stmt}");
        }
    }

    fn show_diagnostics(&self, diagnostics: Diagnostics, src: &str) {
        if diagnostics.has_err() {
            eprintln!("{}", "Parse Error".bold().red());
            eprint!("{}", diagnostics.report(&src))
        }
    }

    fn read_input(&self) -> io::Result<String> {
        let mut input = String::new();
        loop {
            let line = self.read_line()?;
            let line = InputFragment::from_line(&line);
            input += &line.content;
            if line.is_complete {
                break;
            }
            print!("{} ", "→".green().bold());
            io::stdout().flush()?;
        }
        Ok(input)
    }

    fn read_line(&self) -> io::Result<String> {
        let mut line = String::new();
        io::stdin().read_line(&mut line)?;
        Ok(line)
    }
}

struct InputFragment {
    content: String,
    is_complete: bool,
}

impl InputFragment {
    fn from_line(line: &str) -> Self {
        let (content, is_complete) = if let Some(stripped) = line.strip_suffix("\\\r\n") {
            (format!("{}\n", stripped), false) // Windows style
        } else if let Some(stripped) = line.strip_suffix("\\\n") {
            (format!("{}\n", stripped), false) // Unix style
        } else if let Some(stripped) = line.strip_suffix("\\") {
            (stripped.to_string(), false)
        } else {
            (line.to_string(), true)
        };
        Self {
            content,
            is_complete,
        }
    }
}
