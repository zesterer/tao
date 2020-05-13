use std::{
    ops::Range,
    fmt,
};
use parze::region::Region;
use crate::node::SrcNode;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct SrcLoc(usize);

impl SrcLoc {
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

impl fmt::Debug for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<usize> for SrcLoc {
    fn from(pos: usize) -> Self {
        Self(pos)
    }
}

impl From<u64> for SrcLoc {
    fn from(pos: u64) -> Self {
        Self(pos as usize)
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub enum SrcRegion {
    None,
    Range(SrcLoc, SrcLoc),
}

impl SrcRegion {
    pub const fn none() -> Self {
        SrcRegion::None
    }

    pub const fn single(loc: SrcLoc) -> Self {
        SrcRegion::Range(loc, loc.next())
    }

    pub fn range(from: SrcLoc, until: SrcLoc) -> Self {
        if from.0 < until.0 {
            SrcRegion::Range(from, until)
        } else {
            SrcRegion::None
        }
    }

    pub fn contains(self, loc: SrcLoc) -> bool {
        match self {
            SrcRegion::None => false,
            SrcRegion::Range(from, until) => from.0 <= loc.0 && until.0 > loc.0,
        }
    }

    pub fn intersects(self, other: Self) -> bool {
        match (self, other) {
            (SrcRegion::Range(from_a, until_a), SrcRegion::Range(from_b, until_b)) =>
                !(until_a.0 <= from_b.0 || from_a.0 >= until_b.0),
            _ => false,
        }
    }

    pub fn extend_to(self, limit: SrcLoc) -> Self {
        match self {
            SrcRegion::None => SrcRegion::None,
            SrcRegion::Range(from, until) => SrcRegion::Range(from, until.max(limit)),
        }
    }

    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (SrcRegion::None, b) => b,
            (a, SrcRegion::None) => a,
            (SrcRegion::Range(from_a, until_a), SrcRegion::Range(from_b, until_b)) =>
                SrcRegion::Range(from_a.min(from_b), until_a.max(until_b)),
        }
    }

    pub fn homogenize(self, other: Self) -> Self {
        match (self, other) {
            (SrcRegion::None, other) => other,
            (this, SrcRegion::None) => this,
            (this, _) => this,
        }
    }

    pub fn later_than(self, other: Self) -> bool {
        match (self, other) {
            (SrcRegion::Range(from_a, until_a), SrcRegion::Range(from_b, until_b)) =>
                until_a.later_than(until_b),
            _ => false,
        }
    }

    pub fn earliest(self, other: Self) -> Self {
        match (self, other) {
            (SrcRegion::Range(a, _), SrcRegion::Range(b, _)) => if a.later_than(b) {
                other
            } else {
                self
            },
            _ => self,
        }
    }

    pub fn in_context(&self, code: &str) -> Option<((usize, usize), (usize, usize))> {
        match self {
            SrcRegion::Range(from, until) => Some((from.in_context(code), until.in_context(code))),
            SrcRegion::None => None,
        }
    }
}

impl fmt::Debug for SrcRegion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SrcRegion::None => write!(f, "<none>"),
            SrcRegion::Range(from, to) => write!(f, "{:?}:{:?}", from, to),
        }
    }
}

impl From<usize> for SrcRegion {
    fn from(pos: usize) -> Self {
        SrcRegion::Range(SrcLoc::from(pos), SrcLoc::from(pos + 1))
    }
}

impl From<(usize, usize)> for SrcRegion {
    fn from((from, to): (usize, usize)) -> Self {
        SrcRegion::Range(SrcLoc::from(from), SrcLoc::from(to))
    }
}

impl<T: Into<SrcLoc>> From<Range<T>> for SrcRegion {
    fn from(range: Range<T>) -> Self {
        Self::range(range.start.into(), range.end.into())
    }
}

impl Region<char> for SrcRegion {
    fn none() -> Self {
        SrcRegion::none()
    }

    fn single(index: usize, _sym: &char) -> Self {
        SrcRegion::single(index.into())
    }

    fn group(_syms: &[char], range: Range<usize>) -> Self {
        Self::range(range.start.into(), range.end.into())
    }
}

impl<T> Region<SrcNode<T>> for SrcRegion {
    fn none() -> Self {
        SrcRegion::none()
    }

    fn single(index: usize, sym: &SrcNode<T>) -> Self {
        sym.region()
    }

    fn group(syms: &[SrcNode<T>], _range: Range<usize>) -> Self {
        syms
            .first()
            .map(|s| s.region())
            .unwrap_or(SrcRegion::none())
            .union(syms
                .last()
                .map(|s| s.region())
                .unwrap_or(SrcRegion::none()))
    }
}
