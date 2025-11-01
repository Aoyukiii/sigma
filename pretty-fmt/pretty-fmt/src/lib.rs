pub mod impls;
use std::fmt;

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

    pub fn write_levelled_indent(&self, writer: &mut fmt::Formatter) -> std::fmt::Result {
        write!(writer, "{}", self.indent.repeat(self.level))
    }
}

pub struct WithContext<'a, T: PrettyFmt + ?Sized> {
    value: &'a T,
    ctx: &'a PrettyContext<'a>,
}

impl<'a, T> fmt::Display for WithContext<'a, T>
where
    T: PrettyFmt + ?Sized,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.pretty_fmt_with_ctx(self.ctx, f)
    }
}

pub trait PrettyFmt {
    fn pretty_fmt_with_ctx(&self, ctx: &PrettyContext, f: &mut fmt::Formatter) -> std::fmt::Result;

    fn pretty_fmt(&self, f: &mut fmt::Formatter) -> std::fmt::Result {
        self.pretty_fmt_with_ctx(&mut PrettyContext::new(), f)
    }

    fn with_ctx<'a>(&'a self, ctx: &'a PrettyContext) -> WithContext<'a, Self> {
        WithContext { value: self, ctx }
    }
}

pub struct NodeFormatter<'a, 'b, 'c> {
    ctx: &'a PrettyContext<'b>,
    writer: &'a mut fmt::Formatter<'c>,
}

impl<'a, 'b, 'c> NodeFormatter<'a, 'b, 'c> {
    pub fn new(ctx: &'a PrettyContext<'b>, writer: &'a mut fmt::Formatter<'c>) -> Self {
        Self { ctx, writer }
    }

    pub fn header(self, header: &'a str) -> Result<Self, fmt::Error> {
        writeln!(self.writer, "{}(", header)?;
        Ok(self)
    }

    pub fn field(self, key: &'a str, value: &impl PrettyFmt) -> Result<Self, fmt::Error> {
        let ctx = self.ctx.indent();
        ctx.write_levelled_indent(self.writer)?;
        write!(self.writer, "{}: ", key)?;
        value.pretty_fmt_with_ctx(&ctx, self.writer)?;
        writeln!(self.writer, ",")?;
        Ok(self)
    }

    pub fn content(self, content: &impl PrettyFmt) -> Result<Self, fmt::Error> {
        let ctx = self.ctx.indent();
        ctx.write_levelled_indent(self.writer)?;
        content.pretty_fmt_with_ctx(&ctx, self.writer)?;
        writeln!(self.writer, "")?;
        Ok(self)
    }

    pub fn finish(self) -> std::fmt::Result {
        self.ctx.write_levelled_indent(self.writer)?;
        write!(self.writer, ")")
    }
}
