use std::{fmt::Display, ops::Range};

use logos::{Lexer as LogosLexer, Logos};

use crate::core::{diagnostics::errors::LexError, utils::span::Span};

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: Result<TokenKind<'a>, LexError>,
    pub span: Span,
}

impl<'a> Token<'a> {
    /// Get a tuple of [`TokenKind`] and [`Span`], and may panic.
    pub fn unwrap_kind(self) -> (TokenKind<'a>, Span) {
        (self.kind.unwrap(), self.span)
    }

    /// Get a tuple of [`LexError`] and [`Span`], and may panic.
    pub fn unwrap_error(self) -> (LexError, Span) {
        (self.kind.unwrap_err(), self.span)
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token({:?} @ {})", self.kind, self.span)
    }
}

impl<'a> From<(Result<TokenKind<'a>, LexError>, Range<usize>)> for Token<'a> {
    fn from((kind, span): (Result<TokenKind<'a>, LexError>, Range<usize>)) -> Self {
        Self {
            kind,
            span: span.into(),
        }
    }
}

fn slice_str_callback<'a>(lex: &mut LogosLexer<'a, TokenKind<'a>>) -> &'a str {
    lex.slice()
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip(r"[ \r\t\n\f]+"))]
#[logos(error(LexError, LexError::from_lexer))]
pub enum TokenKind<'a> {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("**")]
    DStar,
    #[token("!")]
    Not,
    #[token("->")]
    Arrow,
    #[token("=>")]
    DArrow,
    #[token("=")]
    Eq,
    #[token(":=")]
    ColonEq,
    #[token("|>")]
    Pipe,
    #[token(";")]
    Semicolon,
    #[token("let")]
    KwLet,
    #[token("def")]
    KwDef,
    #[token("in")]
    KwIn,
    #[token("Atom")]
    KwAtom,
    #[token("Type")]
    KwType,

    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_]*", slice_str_callback)]
    Atom(&'a str),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", slice_str_callback)]
    Ident(&'a str),

    EOF,
}

impl<'a> Display for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LParen => write!(f, "`(`"),
            Self::RParen => write!(f, "`)`"),
            Self::Comma => write!(f, "`,`"),
            Self::Dot => write!(f, "`.`"),
            Self::Colon => write!(f, "`:`"),
            Self::Plus => write!(f, "`+`"),
            Self::Minus => write!(f, "`-`"),
            Self::Star => write!(f, "`*`"),
            Self::Slash => write!(f, "`/`"),
            Self::Percent => write!(f, "`%`"),
            Self::DStar => write!(f, "`**`"),
            Self::Not => write!(f, "`!`"),
            Self::Arrow => write!(f, "`->`"),
            Self::DArrow => write!(f, "`=>`"),
            Self::Eq => write!(f, "`=`"),
            Self::ColonEq => write!(f, "`:=`"),
            Self::Pipe => write!(f, "`|>`"),
            Self::Semicolon => write!(f, "`;`"),
            Self::KwLet => write!(f, "keyword `let`"),
            Self::KwDef => write!(f, "keyword `def`"),
            Self::KwIn => write!(f, "keyword `in`"),
            Self::KwAtom => write!(f, "keyword `Atom`"),
            Self::KwType => write!(f, "keyword `Type`"),
            Self::Atom(it) => write!(f, "atom literal {it}"),
            Self::Ident(it) => write!(f, "identifier {it}"),
            Self::EOF => write!(f, "EOF"),
        }
    }
}
