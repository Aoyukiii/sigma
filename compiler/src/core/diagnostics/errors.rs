use std::fmt::Display;

use crate::core::{syntax::lexer::token::TokenKind, utils::span::Span};
use logos::Lexer as LogosLexer;

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
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
                write!(f, "Non-associative operator `{op_str}` used continuously")
            }
            ParseErrorKind::InternalError => {
                write!(f, "Internal error")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    BadToken { tok_str: String, span: Span },
    Unknown,
}

impl Default for LexError {
    fn default() -> Self {
        Self::Unknown
    }
}

impl LexError {
    pub fn from_lexer<'a>(lex: &mut LogosLexer<'a, TokenKind<'a>>) -> Self {
        Self::BadToken {
            tok_str: lex.slice().to_string(),
            span: lex.span().into(),
        }
    }
}
