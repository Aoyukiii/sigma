use std::fmt::{Display, Formatter};

pub trait DisplayReport<C>: Sized {
    fn fmt(&self, f: &mut Formatter, ctx: &C) -> std::fmt::Result;

    fn report<'a>(&'a self, ctx: &'a C) -> Report<'a, C, Self> {
        Report {
            ctx,
            diagnostics: &self,
        }
    }
}

pub struct Report<'a, C, D: DisplayReport<C>> {
    ctx: &'a C,
    diagnostics: &'a D,
}

impl<'a, C, D: DisplayReport<C>> Display for Report<'a, C, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.diagnostics.fmt(f, self.ctx)
    }
}
