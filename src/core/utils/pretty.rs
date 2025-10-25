use std::fmt::Write;

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

    pub fn indented(&'a self) -> Self {
        Self {
            indent: self.indent,
            level: self.level + 1,
        }
    }

    pub fn write_indent(&mut self, writer: &mut impl Write) -> std::fmt::Result {
        write!(writer, "{}", self.indent.repeat(self.level))
    }

    pub fn write_field_ln(
        &mut self,
        writer: &mut impl Write,
        key: &str,
        value: &impl PrettyPrint,
    ) -> std::fmt::Result {
        let mut ctx = self.indented();
        ctx.write_indent(writer)?;
        write!(writer, "{}: ", key)?;
        value.print_ctx(&mut ctx, writer)?;
        writeln!(writer, ",")
    }
}

pub trait PrettyPrint {
    fn print_ctx(&self, ctx: &mut PrettyContext, w: &mut impl Write) -> std::fmt::Result;

    fn print(&self, w: &mut impl Write) -> std::fmt::Result {
        self.print_ctx(&mut PrettyContext::new(), w)
    }
}
