use std::fmt::Display;

use colored::Colorize;

use crate::core::{
    syntax::ast::raw::expr::Expr,
    utils::{
        pretty::{NodeFormatter, PrettyContext, PrettyFmt},
        span::Span,
    },
};

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl From<(StmtKind, Span)> for Stmt {
    fn from((kind, span): (StmtKind, Span)) -> Self {
        Self { kind, span }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @ {}", self.kind, self.span)
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Def(Box<Def>),
    Eval(Box<Expr>),
    Error,
}

impl PrettyFmt for StmtKind {
    fn pretty_fmt_with_ctx(
        &self,
        ctx: &mut PrettyContext,
        w: &mut dyn std::fmt::Write,
    ) -> std::fmt::Result {
        match self {
            Self::Def(it) => {
                NodeFormatter::new(ctx, w)
                    .header("Let")?
                    .field("var", &it.var)?
                    .field("value", &it.value)?
                    .finish()?;
            }
            Self::Eval(it) => {
                NodeFormatter::new(ctx, w)
                    .header("Eval")?
                    .content(it)?
                    .finish()?;
            }
            Self::Error => {
                write!(w, "{}", "Error".red())?;
            }
        }
        Ok(())
    }

    fn pretty_fmt(&self, w: &mut dyn std::fmt::Write) -> std::fmt::Result {
        write!(w, "{} ", "[Stmt]".yellow())?;
        self.pretty_fmt_with_ctx(&mut PrettyContext::new(), w)
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_fmt(f)
    }
}

#[derive(Debug)]
pub struct Def {
    pub var: Box<Expr>,
    pub value: Box<Expr>,
}
