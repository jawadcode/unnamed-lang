use std::fmt;
use std::ops::{Index, Range};

use super::token_kind::TokenKind;

#[derive(Clone, Copy, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[inline(always)]
    /// Returns the length of the token
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline(always)]
    /// Returns the token's text as a slice of the input string
    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} ({}, {})",
            self.kind, self.span.start, self.span.end
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Custom span to store the position of a token in a source string
pub struct Span {
    /// inclusive
    pub start: usize,
    /// exclusive
    pub end: usize,
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}
