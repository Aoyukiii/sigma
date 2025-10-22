use std::{
    fmt::{Debug, Display, Write},
    ops::Range,
};

use colored::Colorize;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Cursor {
    pub line: usize,
    pub col: usize,
}

impl Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

pub trait ToCursor {
    fn to_cursor(self, pos: usize) -> Cursor;
}

impl ToCursor for &str {
    fn to_cursor(self, pos: usize) -> Cursor {
        let mut line = 0;
        let mut col = 0;

        for (i, ch) in self.char_indices() {
            if i >= pos {
                break;
            }

            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        Cursor { line, col }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    /// Convert into two cursors.
    pub fn to_cursors(self, src: &str) -> (Cursor, Cursor) {
        (src.to_cursor(self.start), src.to_cursor(self.end))
    }
}

fn digits_unsigned<T>(n: T) -> usize
where
    T: Copy + PartialOrd + From<u8> + std::ops::DivAssign,
{
    let zero = T::from(0);
    let ten = T::from(10);

    if n == zero {
        return 1;
    }

    let mut count = 0;
    let mut num = n;
    while num > zero {
        count += 1;
        num /= ten;
    }
    count
}

fn write_underline_ln(w: &mut impl Write, begin: usize, end: usize) -> std::fmt::Result {
    writeln!(
        w,
        "{}{}",
        " ".repeat(begin),
        "^".repeat((end - begin).max(1)).bold().red()
    )
}

pub fn write_codeblock(w: &mut impl Write, src: &str, span: Span) -> std::fmt::Result {
    let lines: Vec<_> = src.split("\n").collect();
    let (begin, end) = span.to_cursors(src);
    let lines = &lines[begin.line..=end.line];

    if lines.is_empty() {
        return Ok(());
    }

    let line_num_width = digits_unsigned(end.line);
    let total_lines = lines.len();

    for (i, line) in lines.iter().enumerate() {
        let line_num = begin.line + i + 1;
        let formatted_line_num =
            format!("{:>width$} | ", line_num, width = line_num_width).bright_black();

        // code line
        writeln!(w, "{}{}", formatted_line_num, line)?;

        // padding
        write!(w, "{}", " ".repeat(formatted_line_num.len()))?;

        let (underline_start, underline_end) = match (i, total_lines) {
            (0, 1) => (begin.col, end.col),                 // single line
            (0, _) => (begin.col, line.len()),              // first line
            (i, _) if i == total_lines - 1 => (0, end.col), // last line
            _ => (0, line.len()),                           // other lines
        };

        write_underline_ln(w, underline_start, underline_end)?;
    }

    Ok(())
}

impl Default for Span {
    fn default() -> Self {
        Self { start: 0, end: 0 }
    }
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

    pub fn write_field(
        &mut self,
        writer: &mut impl Write,
        key: &str,
        value: &impl PrettyPrint,
    ) -> std::fmt::Result {
        write!(writer, "{}: ", key)?;
        value.print_ctx(self, writer)?;
        write!(writer, ",")
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
