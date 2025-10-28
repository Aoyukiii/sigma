use crate::core::{
    syntax::ast::elaborated::id::{DebruijnId, ExprId},
    utils::span::Span,
};

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub id: ExprId,
}

#[derive(Debug, Default)]
pub struct TyExpr {
    kind: ExprKind,
    span: Option<Span>,
}

impl From<(ExprKind, Span, ExprId)> for Expr {
    fn from((kind, span, id): (ExprKind, Span, ExprId)) -> Self {
        Self { kind, span, id }
    }
}

#[derive(Debug, Default)]
pub enum ExprKind {
    Var(DebruijnId),
    Atom,
    Type,
    AtomLiteral(String),
    Lambda(Box<Expr>),
    Application(Box<Application>),
    Let(Box<Let>),
    Error,
    #[default]
    Hole,
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
