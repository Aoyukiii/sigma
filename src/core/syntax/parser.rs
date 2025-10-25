pub mod expr;
pub mod stmt;

use std::marker::PhantomData;

use crate::core::{
    diagnostics::{
        Diagnostics,
        errors::{ParseError, ParseErrorKind},
    },
    syntax::{
        ast::raw::stmt::Stmt,
        lexer::{stream::TokenStream, token::TokenKind},
    },
};

pub struct Parser<'a, T>
where
    T: TokenStream<'a>,
{
    tokens: T,
    errs: Diagnostics,
    _phantom: PhantomData<&'a ()>,
}

impl<'a, T> Parser<'a, T>
where
    T: TokenStream<'a>,
{
    pub fn new(tokens: T) -> Self {
        Self {
            tokens,
            errs: Diagnostics::new(),
            _phantom: PhantomData,
        }
    }

    fn report(&mut self, err: ParseError) {
        self.errs.add_err(err)
    }

    fn expect(&mut self, expected: TokenKind) {
        if !self.tokens.expect(expected.clone()) {
            let peek = self.tokens.next();
            let found = match peek.kind {
                Ok(tok) => tok.to_string(),
                Err(_) => "invalid token".to_string(), // TODO: not a good information
            };
            self.report(
                (
                    ParseErrorKind::Expected {
                        expected: expected.to_string(),
                        found,
                    },
                    peek.span,
                )
                    .into(),
            );
        }
    }

    pub fn parse(&mut self) -> (Vec<Stmt>, Diagnostics) {
        self.errs.clear();
        (self.stmts(), std::mem::take(&mut self.errs))
    }
}
