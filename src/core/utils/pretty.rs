use std::{fmt::Write, ops::Deref};

pub struct PrettyContext<'a> {
    indent: &'a str,
    level: usize,
}

impl<'a> PrettyContext<'a> {
    pub fn new() -> Self {
        Self {
            indent: "    ",
            level: 0,
        }
    }

    pub fn indent(&'a self) -> Self {
        Self {
            indent: self.indent,
            level: self.level + 1,
        }
    }

    pub fn write_levelled_indent(&mut self, writer: &mut impl Write) -> std::fmt::Result {
        write!(writer, "{}", self.indent.repeat(self.level))
    }

    pub fn write_field_ln<T>(
        &mut self,
        writer: &mut impl Write,
        key: &str,
        value: &impl Deref<Target = T>,
    ) -> std::fmt::Result
    where
        T: PrettyFmt,
    {
        let mut ctx = self.indent();
        ctx.write_levelled_indent(writer)?;
        write!(writer, "{}: ", key)?;
        value.pretty_fmt_with_ctx(&mut ctx, writer)?;
        writeln!(writer, ",")
    }
}

pub trait PrettyFmt {
    fn pretty_fmt_with_ctx(&self, ctx: &mut PrettyContext, w: &mut impl Write) -> std::fmt::Result;

    fn pretty_fmt(&self, f: &mut impl Write) -> std::fmt::Result {
        self.pretty_fmt_with_ctx(&mut PrettyContext::new(), f)
    }
}
