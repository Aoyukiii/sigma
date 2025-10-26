use std::fmt::{Display, Write};

use colored::Colorize;

use crate::core::utils::span::Span;

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

fn write_underline_ln(w: &mut dyn Write, begin: usize, end: usize) -> std::fmt::Result {
    writeln!(
        w,
        "{}{}",
        " ".repeat(begin),
        "^".repeat((end - begin).max(1)).bold().red()
    )
}

pub fn write_codeblock(w: &mut dyn Write, src: &str, span: Span) -> std::fmt::Result {
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
