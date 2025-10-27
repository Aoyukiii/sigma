use crate::core::{
    syntax::ast::elaborated::id::{DebruijnId, ExprId},
    utils::span::Span,
};

#[derive(Debug)]
pub struct Expr {
    kind: ExprKind,
    span: Span,
    id: ExprId,
}

#[derive(Debug)]
pub enum ExprKind {
    Var(DebruijnId),
    Atom,
    Type,
    AtomLiteral(String),
    Lambda(Box<Expr>),
    Application(Box<Application>),
    Let(Box<Let>),
    Error,
}

#[derive(Debug)]
pub struct Let {
    pub var: DebruijnId,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Application {
    pub func: Box<Expr>,
    pub arg: Box<Expr>,
}
