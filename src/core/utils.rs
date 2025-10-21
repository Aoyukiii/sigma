use std::{
    fmt::{Debug, Display, Write},
    ops::Range,
};

use colored::Colorize;

#[derive(Clone, Copy, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}..{})", self.start, self.end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            format!("{}..{}", self.start, self.end).to_string().yellow()
        )
    }
}

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

    pub fn write_field(&mut self, writer: &mut impl Write, key: &str, value: &impl PrettyPrint) -> std::fmt::Result {
        write!(writer, "{}: ", key)?;
        value.pretty_print(self, writer)?;
        write!(writer, ",")
    }

    pub fn write_field_ln(&mut self, writer: &mut impl Write, key: &str, value: &impl PrettyPrint) -> std::fmt::Result {
        let mut ctx = self.indented();
        ctx.write_indent(writer)?;
        write!(writer, "{}: ", key)?;
        value.pretty_print(&mut ctx, writer)?;
        writeln!(writer, ",")
    }
    
}

pub trait PrettyPrint {
    fn pretty_print(&self, ctx: &mut PrettyContext, w: &mut impl Write) -> std::fmt::Result;

    fn print(&self, w: &mut impl Write) -> std::fmt::Result {
        self.pretty_print(&mut PrettyContext::new(), w)
    }
}
