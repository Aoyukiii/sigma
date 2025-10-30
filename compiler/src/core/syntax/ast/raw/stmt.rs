use std::fmt::Display;

use colored::Colorize;

use crate::core::{syntax::ast::raw::expr::RawExpr, utils::span::Span};

use pretty_fmt::{NodeFormatter, PrettyContext, PrettyFmt};

#[derive(Debug)]
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
        write!(f, "{} @ {}", self.kind, self.span)
    }
}

#[derive(Debug)]
pub enum RawStmtKind {
    Def(Box<Def>),
    Eval(Box<RawExpr>),
    Error,
}

impl PrettyFmt for RawStmtKind {
    fn pretty_fmt_with_ctx(
        &self,
        ctx: &mut PrettyContext,
        f: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        match self {
            Self::Def(it) => {
                NodeFormatter::new(ctx, f)
                    .header("Let")?
                    .field("var", &it.var)?
                    .field("value", &it.value)?
                    .finish()?;
            }
            Self::Eval(it) => {
                NodeFormatter::new(ctx, f)
                    .header("Eval")?
                    .content(it)?
                    .finish()?;
            }
            Self::Error => {
                write!(f, "{}", "Error".red())?;
            }
        }
        Ok(())
    }

    fn pretty_fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ", "[Stmt]".yellow())?;
        self.pretty_fmt_with_ctx(&mut PrettyContext::new(), f)
    }
}

impl Display for RawStmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug)]
pub struct Def {
    pub var: Box<RawExpr>,
    pub value: Box<RawExpr>,
}
