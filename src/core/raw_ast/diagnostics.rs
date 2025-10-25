use colored::Colorize;
use std::fmt::Display;

use crate::core::{
    report::DisplayReport,
    token::lexer::LexError,
    utils::{Span, write_codeblock},
};

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

impl From<(ParseErrorKind, Span)> for ParseError {
    fn from((kind, span): (ParseErrorKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl ParseError {
    fn unknown(span: Span) -> Self {
        Self {
            kind: ParseErrorKind::InternalError,
            span,
        }
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Expected { expected: String, found: String },
    Unexpected { unexpected: String },
    BadToken { tok_str: String },
    UnclosedLParen,
    UnclosedRParen,
    NonAssociativeOp { op_str: String },
    InternalError,
}

impl From<LexError> for ParseError {
    fn from(lex_err: LexError) -> Self {
        match lex_err {
            LexError::BadToken { tok_str, span } => Self {
                kind: ParseErrorKind::BadToken { tok_str },
                span,
            },
            LexError::Unknown => Self::unknown(Span::default()),
        }
    }
}

impl ParseErrorKind {
    pub fn new_expected_expr(found: String) -> Self {
        Self::Expected {
            expected: "expression".to_string(),
            found,
        }
    }
}

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
    fn fmt(&self, w: &mut impl std::fmt::Write, ctx: &&str) -> std::fmt::Result {
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

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParseErrorKind::Expected { expected, found } => {
                write!(f, "Expect {expected} but found {found}")
            }
            ParseErrorKind::Unexpected { unexpected } => {
                write!(f, "Unexpected {unexpected}")
            }
            ParseErrorKind::BadToken { tok_str } => {
                write!(f, "Bad token {tok_str}")
            }
            ParseErrorKind::UnclosedLParen => {
                write!(f, "Uncolsed `(`")
            }
            ParseErrorKind::UnclosedRParen => {
                write!(f, "Unclosed `)`")
            }
            ParseErrorKind::NonAssociativeOp { op_str } => {
                write!(f, "Non-associative operator {op_str} used continuously")
            }
            ParseErrorKind::InternalError => {
                write!(f, "Internal error")
            }
        }
    }
}
