use colored::Colorize;

use crate::core::{
    diagnostics::{display_report::DisplayReport, errors::ParseError},
    utils::cursor::write_codeblock,
};

pub mod display_report;
pub mod errors;

#[derive(Debug)]
pub struct Diagnostics {
    errs: Vec<ParseError>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self { errs: Vec::new() }
    }

    pub fn add_err(&mut self, err: ParseError) {
        self.errs.push(err)
    }

    pub fn has_err(&self) -> bool {
        !self.errs.is_empty()
    }

    pub fn clear(&mut self) {
        self.errs.clear()
    }
}

impl Default for Diagnostics {
    fn default() -> Self {
        Self::new()
    }
}

impl DisplayReport<&str> for Diagnostics {
    fn fmt(&self, w: &mut dyn std::fmt::Write, ctx: &&str) -> std::fmt::Result {
        let errs = &self.errs;
        for err in errs {
            writeln!(
                w,
                "{} {}",
                format!("[repl:{}]", err.span.to_cursors(ctx).0).underline(),
                err.to_string().red()
            )?;
            write_codeblock(w, ctx, err.span)?;
        }
        Ok(())
    }
}
