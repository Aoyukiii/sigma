use std::fmt::Display;

use colored::Colorize;

use crate::core::{
    raw_ast::expr::Expr,
    utils::{PrettyContext, PrettyPrint, Span},
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

impl PrettyPrint for StmtKind {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::Def(it) => {
                writeln!(w, "Let(")?;
                ctx.write_field_ln(w, "var", it.var.as_ref())?;
                ctx.write_field_ln(w, "value", it.value.as_ref())?;
                ctx.write_indent(w)?;
                write!(w, ")")?;
            }
            Self::Eval(it) => {
                writeln!(w, "Eval(")?;
                let mut indented = ctx.indented();
                indented.write_indent(w)?;
                it.print_ctx(&mut ctx.indented(), w)?;
                writeln!(w, "")?;
                ctx.write_indent(w)?;
                write!(w, ")")?;
            }
            Self::Error => {
                write!(w, "{}", "Error".red())?;
            }
        }
        Ok(())
    }

    fn print(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(w, "{} ", "[Stmt]".yellow())?;
        self.print_ctx(&mut PrettyContext::new(), w)
    }
}

impl Display for StmtKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(f)
    }
}

#[derive(Debug)]
pub struct Def {
    pub var: Box<Expr>,
    pub value: Box<Expr>,
}
