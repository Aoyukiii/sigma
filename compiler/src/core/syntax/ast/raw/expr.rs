use std::fmt::Display;

use colored::Colorize;
use pretty_fmt_macros::PrettyFmt;

use crate::core::{
    syntax::ast::raw::operator::{Infix, Prefix},
    utils::span::Span,
};

use pretty_fmt::PrettyFmt;

#[derive(Debug, PrettyFmt)]
#[pretty_fmt("{} @ {}", kind.with_ctx(ctx), span)]
pub struct RawExpr {
    pub kind: RawExprKind,
    pub span: Span,
}

impl From<(RawExprKind, Span)> for RawExpr {
    fn from((kind, span): (RawExprKind, Span)) -> Self {
        Self { kind, span }
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
    #[pretty_fmt("{}", arg0.magenta())]
    Ident(String),

    #[pretty_fmt("{}", "Atom".yellow())]
    Atom,

    #[pretty_fmt("{}", "Type".yellow())]
    Type,

    #[pretty_fmt("{}", arg0.yellow())]
    AtomLiteral(String),

    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Annotated(Box<Annotated>),

    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Lambda(Box<Lambda>),

    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Application(Box<Application>),

    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Prefix(Box<PrefixExpr>),

    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Infix(Box<InfixExpr>),

    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Let(Box<Let>),

    #[pretty_fmt("{}", "Error".red())]
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
#[header("({}) @ {} ", op.to_string().magenta(), op_span)]
pub struct PrefixExpr {
    #[skip]
    pub op: Prefix,
    #[skip]
    pub op_span: Span,
    pub rhs: Box<RawExpr>,
}

#[derive(Debug, PrettyFmt)]
#[header("({}) @ {} ", op.to_string().magenta(), op_span)]
pub struct InfixExpr {
    #[skip]
    pub op: Infix,
    #[skip]
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
