use std::fmt::Display;

use colored::Colorize;
use pretty_fmt_macros::PrettyFmt;

use crate::core::{
    syntax::ast::raw::operator::{Infix, Prefix},
    utils::span::Span,
};

use pretty_fmt::{NodeFormatter, PrettyContext, PrettyFmt};

#[derive(Debug)]
pub struct RawExpr {
    pub kind: RawExprKind,
    pub span: Span,
}

impl From<(RawExprKind, Span)> for RawExpr {
    fn from((kind, span): (RawExprKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl PrettyFmt for RawExpr {
    fn pretty_fmt_with_ctx(
        &self,
        ctx: &PrettyContext,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        self.kind.pretty_fmt_with_ctx(ctx, f)?;
        write!(f, " @ {}", self.span)
    }
}

impl Display for RawExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

impl RawExpr {
    pub fn new_err(span: Span) -> Self {
        Self {
            kind: RawExprKind::Error,
            span,
        }
    }
}

#[derive(Debug, PrettyFmt)]
pub enum RawExprKind {
    Ident(String),
    Atom,
    Type,
    AtomLiteral(String),
    Annotated(Box<Annotated>),
    Lambda(Box<Lambda>),
    Application(Box<Application>),
    Prefix(Box<PrefixExpr>),
    Infix(Box<InfixExpr>),
    Let(Box<Let>),
    Error,
}

impl Display for RawExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug, PrettyFmt)]
pub struct Annotated {
    pub expr: Box<RawExpr>,
    pub type_expr: Box<RawExpr>,
}

#[derive(Debug, PrettyFmt)]
pub struct Lambda {
    pub param: Box<RawExpr>,
    pub body: Box<RawExpr>,
}

#[derive(Debug, PrettyFmt)]
pub struct Application {
    pub func: Box<RawExpr>,
    pub arg: Box<RawExpr>,
}

#[derive(Debug, PrettyFmt)]
pub struct PrefixExpr {
    pub op: Prefix,
    pub op_span: Span,
    pub rhs: Box<RawExpr>,
}

#[derive(Debug, PrettyFmt)]
pub struct InfixExpr {
    pub op: Infix,
    pub op_span: Span,
    pub lhs: Box<RawExpr>,
    pub rhs: Box<RawExpr>,
}

#[derive(Debug, PrettyFmt)]
pub struct Let {
    pub var: Box<RawExpr>,
    pub value: Box<RawExpr>,
    pub body: Box<RawExpr>,
}
