use std::{fmt::Display, ops::Range};

use logos::{Lexer as LogosLexer, Logos, SpannedIter};

use crate::core::utils::Span;

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub kind: Result<TokenKind<'a>, LexError>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn is_eof(&self) -> bool {
        matches!(&self.kind, Ok(kind) if kind.is_eof())
    }

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

pub struct Lexer<'a> {
    tok_stream: SpannedIter<'a, TokenKind<'a>>,
    current_tok: Token<'a>,
    src: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut tok_stream = TokenKind::lexer(src).spanned();
        let current_tok = tok_stream
            .next()
            .unwrap_or((Ok(TokenKind::EOF), src.len()..src.len()))
            .into();
        Self {
            src,
            tok_stream,
            current_tok,
        }
    }

    /// Consume a token
    pub fn next(&mut self) -> Token<'a> {
        let tok = std::mem::replace(
            &mut self.current_tok,
            self.tok_stream
                .next()
                .unwrap_or((Ok(TokenKind::EOF), self.src.len() - 1..self.src.len() - 1))
                .into(),
        );
        tok
    }

    pub fn is_empty(&self) -> bool {
        self.peek_is(TokenKind::EOF)
    }

    /// Peek a token
    pub const fn peek(&self) -> &Token<'a> {
        &self.current_tok
    }

    /// Peek a token and check if it is expected
    pub fn peek_is(&self, expected: TokenKind) -> bool {
        matches!(&self.peek().kind, Ok(tok) if *tok == expected)
    }

    pub fn expect(&mut self, expected: TokenKind) -> bool {
        let is_expected = self.peek_is(expected);
        if is_expected {
            self.next();
        }
        is_expected
    }
}

fn slice_str_callback<'a>(lex: &mut LogosLexer<'a, TokenKind<'a>>) -> &'a str {
    lex.slice()
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
    fn from_lexer<'a>(lex: &mut LogosLexer<'a, TokenKind<'a>>) -> Self {
        Self::BadToken {
            tok_str: lex.slice().to_string(),
            span: lex.span().into(),
        }
    }
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
    #[token("|>")]
    Pipe,
    #[token(";")]
    Semicolon,
    #[token("let")]
    KwLet,
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
        write!(f, "{:?}", self) // TODO: write a full implementation instead of using Debug
    }
}

impl<'a> TokenKind<'a> {
    pub fn is_eof(&self) -> bool {
        matches!(self, TokenKind::EOF)
    }
}

#[cfg(test)]
mod test {
    use logos::Logos;

    use crate::core::lexer::{Lexer, TokenKind};

    #[test]
    fn lexing() {
        let to_test = [
            (
                "Hi, Atom 'atom.",
                vec![
                    Ok(TokenKind::Ident("Hi")),
                    Ok(TokenKind::Comma),
                    Ok(TokenKind::KwAtom),
                    Ok(TokenKind::Atom("'atom")),
                    Ok(TokenKind::Dot),
                ],
            ),
            (
                "let me: Type you;",
                vec![
                    Ok(TokenKind::KwLet),
                    Ok(TokenKind::Ident("me")),
                    Ok(TokenKind::Colon),
                    Ok(TokenKind::KwType),
                    Ok(TokenKind::Ident("you")),
                    Ok(TokenKind::Semicolon),
                ],
            ),
        ];

        for (source, tokens) in to_test {
            let mut tokens = tokens.into_iter();
            let mut lexer = TokenKind::lexer(source);

            while let Some(token_lexed) = lexer.next() {
                if let Some(token) = tokens.next() {
                    assert_eq!(token_lexed, token)
                } else {
                    panic!("Token length is not enough!")
                }
            }

            if tokens.next() != None {
                panic!("Lexed token length is not enough!")
            }
        }
    }

    #[test]
    fn iter_with_span() {
        let mut lexer = Lexer::new("");
        lexer.next();
    }
}
