use std::iter::Peekable;

use logos::{Lexer as LogosLexer, Logos};

use crate::core::utils::Span;

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

pub fn lexer<'a>(source: &'a str) -> TokenStream<'a> {
    let iter = TokenKind::lexer(source).spanned().map(|(tok, span)| Token {
        kind: if let Ok(tok) = tok {
            tok
        } else {
            TokenKind::Error
        },
        span: (span.start, span.end).into(),
    });

    let boxed: Box<dyn Iterator<Item = Token<'a>>> = Box::new(iter);
    boxed.peekable()
}

pub type TokenStream<'a> = Peekable<Box<dyn Iterator<Item = Token<'a>> + 'a>>;

fn slice_str_callback<'a>(lex: &mut Lexer<'a>) -> &'a str {
    lex.slice()
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip(r"[ \r\t\n\f]+"))]
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
    #[token("*")]
    Star,
    #[token("->")]
    Arrow,
    #[token("=>")]
    DArrow,
    #[token("=")]
    Eq,
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

    Error,
}

pub type Lexer<'a> = LogosLexer<'a, TokenKind<'a>>;

#[cfg(test)]
mod test {
    use logos::Logos;

    use crate::core::lexer::{Token, TokenKind, lexer};

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
        let mut lexer = lexer("");
        lexer.next();
    }
}
