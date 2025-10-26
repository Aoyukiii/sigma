use std::fmt::Display;

use colored::Colorize;

use crate::core::{
    syntax::ast::raw::operator::{Infix, Prefix},
    utils::{
        pretty::{NodeFormatter, PrettyContext, PrettyFmt},
        span::Span,
    },
};

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl From<(ExprKind, Span)> for Expr {
    fn from((kind, span): (ExprKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl PrettyFmt for Expr {
    fn pretty_fmt_with_ctx(
        &self,
        ctx: &mut PrettyContext,
        w: &mut dyn std::fmt::Write,
    ) -> std::fmt::Result {
        self.kind.pretty_fmt_with_ctx(ctx, w)?;
        write!(w, " @ {}", self.span)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

impl Expr {
    pub fn new_err(span: Span) -> Self {
        Self {
            kind: ExprKind::Error,
            span,
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
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

impl PrettyFmt for ExprKind {
    fn pretty_fmt_with_ctx(
        &self,
        ctx: &mut PrettyContext,
        w: &mut dyn std::fmt::Write,
    ) -> std::fmt::Result {
        match self {
            Self::Ident(it) => write!(w, "{}", it.to_string().magenta()),
            Self::Atom => write!(w, "{}", "Atom".yellow()),
            Self::Type => write!(w, "{}", "Type".yellow()),
            Self::AtomLiteral(it) => write!(w, "{}", it.yellow()),

            Self::Annotated(it) => NodeFormatter::new(ctx, w)
                .header("Annotated")?
                .field("expr", &it.expr)?
                .field("type", &it.type_expr)?
                .finish(),
            Self::Application(it) => NodeFormatter::new(ctx, w)
                .header("Application")?
                .field("func", &it.func)?
                .field("arg", &it.arg)?
                .finish(),
            Self::Lambda(it) => NodeFormatter::new(ctx, w)
                .header("Lambda")?
                .field("param", &it.param)?
                .field("body", &it.body)?
                .finish(),
            Self::Prefix(it) => NodeFormatter::new(ctx, w)
                .header(&format!(
                    "({}) @ {}",
                    it.op.to_string().magenta(),
                    it.op_span
                ))?
                .field("op_span", &it.op_span)?
                .field("rhs", &it.rhs)?
                .finish(),
            Self::Infix(it) => NodeFormatter::new(ctx, w)
                .header(&format!(
                    "({}) @ {} ",
                    it.op.to_string().magenta(),
                    it.op_span
                ))?
                .field("op_span", &it.op_span)?
                .field("lhs", &it.lhs)?
                .field("rhs", &it.rhs)?
                .finish(),
            Self::Let(it) => NodeFormatter::new(ctx, w)
                .header("Let")?
                .field("var", &it.var)?
                .field("value", &it.value)?
                .field("body", &it.body)?
                .finish(),
            Self::Error => write!(w, "{}", "Error".red()),
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug)]
pub struct Annotated {
    pub expr: Box<Expr>,
    pub type_expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Lambda {
    pub param: Box<Expr>,
    pub body: Box<Expr>,
}

#[derive(Debug)]
pub struct Application {
    pub func: Box<Expr>,
    pub arg: Box<Expr>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub op: Prefix,
    pub op_span: Span,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct InfixExpr {
    pub op: Infix,
    pub op_span: Span,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug)]
pub struct Let {
    pub var: Box<Expr>,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}
