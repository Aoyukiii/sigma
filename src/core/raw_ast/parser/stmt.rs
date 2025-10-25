use crate::core::{
    raw_ast::{
        Parser,
        stmt::{Def, Stmt, StmtKind},
    },
    token::{lexer::TokenKind, stream::TokenStream},
    utils::Span,
};

impl<'a, T> Parser<'a, T>
where
    T: TokenStream<'a>,
{
    pub fn stmts(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.tokens.is_empty() {
            let peek = &self.tokens.peek().kind;
            let stmt = match peek {
                Ok(TokenKind::KwDef) => {
                    let span = self.tokens.next().span;
                    self.stmt_def(span)
                }
                Ok(_) => break,
                Err(_) => {
                    let (e, span) = self.tokens.next().unwrap_error();
                    self.report(e.into());
                    (StmtKind::Error, span).into()
                }
            };
            stmts.push(stmt);
        }
        stmts
    }

    pub fn stmt_def(&mut self, let_span: Span) -> Stmt {
        let var = Box::new(self.expr());
        self.expect(TokenKind::ColonEq);
        let value = Box::new(self.expr());
        let span = let_span.merge(value.span);
        (StmtKind::Def(Box::new(Def { var, value })), span).into()
    }
}
