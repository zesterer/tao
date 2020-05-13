use std::{
    ops::Range,
    fmt,
};
use parze::span::Span as ParzeSpan;
use crate::node::SrcNode;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Loc(usize);

impl Loc {
    pub const fn start() -> Self {
        Self(0)
    }

    pub const fn at(index: usize) -> Self {
        Self(index)
    }

    pub fn min(self, other: Self) -> Self {
        Self(self.0.min(other.0))
    }

    pub fn max(self, other: Self) -> Self {
        Self(self.0.max(other.0))
    }

    pub fn in_context(&self, code: &str) -> (usize, usize) {
        let mut pos = self.0;
        for (i, line) in code.lines().enumerate() {
            if pos < line.len() + 1 {
                return (i, pos);
            }
            pos -= line.len() + 1;
        }
        (code.lines().count(), 0)
    }

    pub const fn next(self) -> Self {
        Self(self.0 + 1)
    }

    pub const fn prev(self) -> Self {
        Self(self.0 - 1)
    }

    pub fn later_than(self, other: Self) -> bool {
        self.0 > other.0
    }
}

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<usize> for Loc {
    fn from(pos: usize) -> Self {
        Self(pos)
    }
}

impl From<u64> for Loc {
    fn from(pos: u64) -> Self {
        Self(pos as usize)
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum Span {
    None,
    Range(Loc, Loc),
}

impl Span {
    pub const fn none() -> Self {
        Span::None
    }

    pub const fn single(loc: Loc) -> Self {
        Span::Range(loc, loc.next())
    }

    pub fn range(from: Loc, until: Loc) -> Self {
        if from.0 < until.0 {
            Span::Range(from, until)
        } else {
            Span::None
        }
    }

    pub fn contains(self, loc: Loc) -> bool {
        match self {
            Span::None => false,
            Span::Range(from, until) => from.0 <= loc.0 && until.0 > loc.0,
        }
    }

    pub fn intersects(self, other: Self) -> bool {
        match (self, other) {
            (Span::Range(from_a, until_a), Span::Range(from_b, until_b)) =>
                !(until_a.0 <= from_b.0 || from_a.0 >= until_b.0),
            _ => false,
        }
    }

    pub fn extend_to(self, limit: Loc) -> Self {
        match self {
            Span::None => Span::None,
            Span::Range(from, until) => Span::Range(from, until.max(limit)),
        }
    }

    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (Span::None, b) => b,
            (a, Span::None) => a,
            (Span::Range(from_a, until_a), Span::Range(from_b, until_b)) =>
                Span::Range(from_a.min(from_b), until_a.max(until_b)),
        }
    }

    pub fn homogenize(self, other: Self) -> Self {
        match (self, other) {
            (Span::None, other) => other,
            (this, Span::None) => this,
            (this, _) => this,
        }
    }

    pub fn later_than(self, other: Self) -> bool {
        match (self, other) {
            (Span::Range(from_a, until_a), Span::Range(from_b, until_b)) =>
                until_a.later_than(until_b),
            _ => false,
        }
    }

    pub fn earliest(self, other: Self) -> Self {
        match (self, other) {
            (Span::Range(a, _), Span::Range(b, _)) => if a.later_than(b) {
                other
            } else {
                self
            },
            _ => self,
        }
    }

    pub fn in_context(&self, code: &str) -> Option<((usize, usize), (usize, usize))> {
        match self {
            Span::Range(from, until) => Some((from.in_context(code), until.in_context(code))),
            Span::None => None,
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Span::None => write!(f, "<none>"),
            Span::Range(from, to) => write!(f, "{:?}:{:?}", from, to),
        }
    }
}

impl From<usize> for Span {
    fn from(pos: usize) -> Self {
        Span::Range(Loc::from(pos), Loc::from(pos + 1))
    }
}

impl From<(usize, usize)> for Span {
    fn from((from, to): (usize, usize)) -> Self {
        Span::Range(Loc::from(from), Loc::from(to))
    }
}

impl<T: Into<Loc>> From<Range<T>> for Span {
    fn from(range: Range<T>) -> Self {
        Self::range(range.start.into(), range.end.into())
    }
}

impl ParzeSpan<char> for Span {
    fn none() -> Self {
        Span::none()
    }

    fn single(index: usize, _sym: &char) -> Self {
        Span::single(index.into())
    }

    fn group(_syms: &[char], range: Range<usize>) -> Self {
        Self::range(range.start.into(), range.end.into())
    }
}

impl<T> ParzeSpan<SrcNode<T>> for Span {
    fn none() -> Self {
        Span::none()
    }

    fn single(index: usize, sym: &SrcNode<T>) -> Self {
        sym.span()
    }

    fn group(syms: &[SrcNode<T>], _range: Range<usize>) -> Self {
        syms
            .first()
            .map(|s| s.span())
            .unwrap_or(Span::none())
            .union(syms
                .last()
                .map(|s| s.span())
                .unwrap_or(Span::none()))
    }
}
