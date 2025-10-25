use logos::{Logos, SpannedIter};

use crate::core::syntax::lexer::{
    stream::TokenStream,
    token::{Token, TokenKind},
};

pub mod stream;
pub mod token;

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
}

impl<'a> TokenStream<'a> for Lexer<'a> {
    /// Consume a token
    fn next(&mut self) -> Token<'a> {
        let tok = std::mem::replace(
            &mut self.current_tok,
            self.tok_stream
                .next()
                .unwrap_or((Ok(TokenKind::EOF), self.src.len() - 1..self.src.len() - 1))
                .into(),
        );
        tok
    }

    /// Peek a token
    fn peek(&self) -> &Token<'a> {
        &self.current_tok
    }
}
