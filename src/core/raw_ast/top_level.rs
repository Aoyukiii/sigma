use crate::core::raw_ast::{expr::Expr, stmt::Stmt};

#[derive(Debug)]
pub enum TopLevel {
    Stmts(Vec<Stmt>),
    Expr(Expr),
}
