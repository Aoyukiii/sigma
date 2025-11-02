use colored::Colorize;
use pretty_fmt_macros::PrettyFmt;

use crate::core::{syntax::ast::raw::expr::RawExpr, utils::span::Span};

#[derive(Debug, PrettyFmt)]
#[pretty_fmt("{} {} @ {}", "[Stmt]".yellow(), kind.with_ctx(ctx), span)]
#[impl_display]
pub struct RawStmt {
    pub kind: RawStmtKind,
    pub span: Span,
}

impl From<(RawStmtKind, Span)> for RawStmt {
    fn from((kind, span): (RawStmtKind, Span)) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PrettyFmt)]
#[impl_display]
pub enum RawStmtKind {
    #[pretty_fmt("{}", arg0.with_ctx(ctx))]
    Def(Box<Def>),
    Eval(Box<RawExpr>),
    #[pretty_fmt("{}", "Error".red())]
    Error,
}

#[derive(Debug, PrettyFmt)]
pub struct Def {
    pub var: Box<RawExpr>,
    pub value: Box<RawExpr>,
}
