use std::fmt::Display;

use colored::Colorize;

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
        ctx: &mut PrettyContext,
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

#[derive(Debug)]
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

impl PrettyFmt for RawExprKind {
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

impl Display for RawExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug)]
pub struct Annotated {
    pub expr: Box<RawExpr>,
    pub type_expr: Box<RawExpr>,
}

#[derive(Debug)]
pub struct Lambda {
    pub param: Box<RawExpr>,
    pub body: Box<RawExpr>,
}

#[derive(Debug)]
pub struct Application {
    pub func: Box<RawExpr>,
    pub arg: Box<RawExpr>,
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub op: Prefix,
    pub op_span: Span,
    pub rhs: Box<RawExpr>,
}

#[derive(Debug)]
pub struct InfixExpr {
    pub op: Infix,
    pub op_span: Span,
    pub lhs: Box<RawExpr>,
    pub rhs: Box<RawExpr>,
}

#[derive(Debug)]
pub struct Let {
    pub var: Box<RawExpr>,
    pub value: Box<RawExpr>,
    pub body: Box<RawExpr>,
}
