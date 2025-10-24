use crate::core::token::lexer::{Token, TokenKind};

pub trait TokenStream<'a> {
    /// Consume a token
    fn next(&mut self) -> Token<'a>;

    fn is_empty(&self) -> bool {
        self.peek_is(TokenKind::EOF)
    }

    /// Peek a token
    fn peek(&self) -> &Token<'a>;

    /// Peek a token and check if it is expected
    fn peek_is(&self, expected: TokenKind) -> bool {
        matches!(&self.peek().kind, Ok(tok) if *tok == expected)
    }

    fn expect(&mut self, expected: TokenKind) -> bool {
        let is_expected = self.peek_is(expected);
        if is_expected {
            self.next();
        }
        is_expected
    }
}
