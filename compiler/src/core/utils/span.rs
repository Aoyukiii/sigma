use std::{fmt::Debug, ops::Range};

use colored::Colorize;
use pretty_fmt::PrettyFmt;
use pretty_fmt_macros::PrettyFmt;

use crate::core::utils::cursor::{Cursor, ToCursor};

#[derive(Clone, Copy, PartialEq, Eq, PrettyFmt)]
#[pretty_fmt("{}", format!("{}..{}", self.start, self.end).yellow())]
#[impl_display]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
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
