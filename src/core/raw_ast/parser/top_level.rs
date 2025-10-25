use crate::core::{
    raw_ast::{Parser, stmt::StmtKind, top_level::TopLevel},
    token::{lexer::TokenKind, stream::TokenStream},
};

impl<'a, T> Parser<'a, T>
where
    T: TokenStream<'a>,
{
    pub fn top_level(&mut self) -> TopLevel {
        let mut stmts = Vec::new();
        while !self.tokens.is_empty() {
            let peek = &self.tokens.peek().kind;
            let stmt = match peek {
                Ok(TokenKind::KwDef) => {
                    let span = self.tokens.next().span;
                    self.stmt_def(span)
                }
                Ok(_) => return TopLevel::Expr(self.expr()),
                Err(_) => {
                    let (e, span) = self.tokens.next().unwrap_error();
                    self.report(e.into());
                    (StmtKind::Error, span).into()
                }
            };
            stmts.push(stmt);
        }
        TopLevel::Stmts(stmts)
    }
}
