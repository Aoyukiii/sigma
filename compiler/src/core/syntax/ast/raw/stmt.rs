use std::fmt::Display;

use colored::Colorize;
use pretty_fmt_macros::PrettyFmt;

use crate::core::{syntax::ast::raw::expr::RawExpr, utils::span::Span};

use pretty_fmt::PrettyFmt;

#[derive(Debug, PrettyFmt)]
#[pretty_fmt("{} {} @ {}", "[Stmt]".yellow(), kind, span)]
pub struct RawStmt {
    pub kind: RawStmtKind,
    pub span: Span,
}

impl From<(RawStmtKind, Span)> for RawStmt {
    fn from((kind, span): (RawStmtKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl Display for RawStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug, PrettyFmt)]
pub enum RawStmtKind {
    Def(Box<Def>),
    Eval(Box<RawExpr>),
    #[pretty_fmt("{}", "Error".red())]
    Error,
}

impl Display for RawStmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug, PrettyFmt)]
pub struct Def {
    pub var: Box<RawExpr>,
    pub value: Box<RawExpr>,
}
