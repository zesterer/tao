use std::{ops::Range, cmp::Ordering, fmt};
use internment::Intern;
use crate::ast::loader::SrcId;

#[derive(Copy, Clone)]
pub struct Span {
    src: SrcId,
    range: (usize, usize),
}

impl Span {
    pub fn new(src: SrcId, range: Range<usize>) -> Self {
        Self { src, range: (range.start, range.end) }
    }

    pub fn union(self, other: Self) -> Self {
        assert_eq!(self.src, other.src, "Attempted to union spans from independent sources");
        Self {
            src: self.src,
            range: (self.range.0.min(other.range.1)..self.range.1.max(other.range.1)),
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = SrcId;

    fn source(&self) -> Self::SourceId { self.src }
    fn start(&self) -> usize { self.range.0 }
    fn end(&self) -> usize { self.range.1 }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.range {
            Some((a, b)) => write!(f, "{}..{}", a, b),
            None => write!(f, "<?>"),
        }
    }
}
