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
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        self.kind.pretty_fmt_with_ctx(ctx, f)?;
        write!(f, " @ {}", self.span)
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
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        match self {
            Self::Ident(it) => write!(f, "{}", it.to_string().magenta()),
            Self::Atom => write!(f, "{}", "Atom".yellow()),
            Self::Type => write!(f, "{}", "Type".yellow()),
            Self::AtomLiteral(it) => write!(f, "{}", it.yellow()),

            Self::Annotated(it) => NodeFormatter::new(ctx, f)
                .header("Annotated")?
                .field("expr", &it.expr)?
                .field("type", &it.type_expr)?
                .finish(),
            Self::Application(it) => NodeFormatter::new(ctx, f)
                .header("Application")?
                .field("func", &it.func)?
                .field("arg", &it.arg)?
                .finish(),
            Self::Lambda(it) => NodeFormatter::new(ctx, f)
                .header("Lambda")?
                .field("param", &it.param)?
                .field("body", &it.body)?
                .finish(),
            Self::Prefix(it) => NodeFormatter::new(ctx, f)
                .header(&format!(
                    "({}) @ {}",
                    it.op.to_string().magenta(),
                    it.op_span
                ))?
                .field("op_span", &it.op_span)?
                .field("rhs", &it.rhs)?
                .finish(),
            Self::Infix(it) => NodeFormatter::new(ctx, f)
                .header(&format!(
                    "({}) @ {} ",
                    it.op.to_string().magenta(),
                    it.op_span
                ))?
                .field("op_span", &it.op_span)?
                .field("lhs", &it.lhs)?
                .field("rhs", &it.rhs)?
                .finish(),
            Self::Let(it) => NodeFormatter::new(ctx, f)
                .header("Let")?
                .field("var", &it.var)?
                .field("value", &it.value)?
                .field("body", &it.body)?
                .finish(),
            Self::Error => write!(f, "{}", "Error".red()),
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
