use super::*;
use std::{
    ops::Range,
    fmt,
};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    src: SrcId,
    range: (usize, usize),
}

impl Span {
    #[cfg(test)]
    pub fn empty() -> Self {
        Self::new(SrcId::empty(), 0..0)
    }

    pub fn src(&self) -> SrcId { self.src }

    pub fn range(&self) -> Range<usize> { self.start()..self.end() }

    pub fn union(self, other: Self) -> Self {
        assert_eq!(self.src, other.src, "attempted to union spans with different sources");
        Self {
            range: (self.start().min(other.start()), self.end().max(other.end())),
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.src, self.range())
    }
}

impl chumsky::Span for Span {
    type Context = SrcId;
    type Offset = usize;

    fn new(src: SrcId, range: Range<usize>) -> Self {
        assert!(range.start <= range.end);
        Self { src, range: (range.start, range.end) }
    }

    fn context(&self) -> SrcId { self.src }
    fn start(&self) -> Self::Offset { self.range.0 }
    fn end(&self) -> Self::Offset { self.range.1 }
}

impl ariadne::Span for Span {
    type SourceId = SrcId;

    fn source(&self) -> &SrcId { &self.src }

    fn start(&self) -> usize { self.range.0 }
    fn end(&self) -> usize { self.range.1 }
}
