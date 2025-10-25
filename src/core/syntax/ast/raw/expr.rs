use std::fmt::Display;

use colored::Colorize;

use crate::core::{
    syntax::ast::raw::operator::{Infix, Prefix},
    utils::{
        pretty::{PrettyContext, PrettyPrint},
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

impl PrettyPrint for Expr {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        self.kind.print_ctx(ctx, w)?;
        write!(w, " @ {}", self.span)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
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

impl PrettyPrint for ExprKind {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::Ident(it) => write!(w, "{}", it.to_string().magenta()),
            Self::Atom => write!(w, "{}", "Atom".yellow()),
            Self::Type => write!(w, "{}", "Type".yellow()),
            Self::AtomLiteral(it) => write!(w, "{}", it.yellow()),
            Self::Annotated(it) => {
                writeln!(w, "Annotated(")?;
                ctx.write_field_ln(w, "expr", it.expr.as_ref())?;
                ctx.write_field_ln(w, "type", it.type_expr.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Application(it) => {
                writeln!(w, "Applicaion(")?;
                ctx.write_field_ln(w, "func", it.func.as_ref())?;
                ctx.write_field_ln(w, "arg", it.arg.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Lambda(it) => {
                writeln!(w, "Lambda(")?;
                ctx.write_field_ln(w, "param", it.param.as_ref())?;
                ctx.write_field_ln(w, "body", it.body.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Prefix(it) => {
                writeln!(w, "({}) @ {} (", it.op.to_string().magenta(), it.op_span)?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Infix(it) => {
                writeln!(w, "({}) @ {} (", it.op.to_string().magenta(), it.op_span)?;
                ctx.write_field_ln(w, "lhs", it.lhs.as_ref())?;
                ctx.write_field_ln(w, "rhs", it.rhs.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Let(it) => {
                writeln!(w, "Let(")?;
                ctx.write_field_ln(w, "var", it.var.as_ref())?;
                ctx.write_field_ln(w, "value", it.value.as_ref())?;
                ctx.write_field_ln(w, "body", it.body.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")
            }
            Self::Error => write!(w, "{}", "Error".red()),
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
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
