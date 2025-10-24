use std::fmt::{Display, Write};

pub trait DisplayReport<C>: Sized {
    fn fmt(&self, w: &mut impl Write, ctx: &C) -> std::fmt::Result;

    fn report<'a>(&'a self, ctx: &'a C) -> Report<'a, C, Self> {
        Report {
            ctx,
            diagnostics: &self,
        }
    }
}

#[derive(Debug)]
pub struct Report<'a, C, D: DisplayReport<C>> {
    ctx: &'a C,
    diagnostics: &'a D,
}

impl<'a, C, D: DisplayReport<C>> Display for Report<'a, C, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.diagnostics.fmt(f, self.ctx)
    }
}
