use super::*;
use std::{
    ops::Range,
    fmt,
};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span {
    src: SrcId,
    range: Option<(usize, usize)>,
}

impl Span {
    #[cfg(test)]
    pub fn empty() -> Self {
        Self::new(SrcId::empty(), None)
    }

    pub fn new(src: SrcId, range: Option<Range<usize>>) -> Self {
        Self {
            src,
            range: range.map(|range| (range.start, range.end)),
        }
    }

    pub fn range(&self) -> Option<Range<usize>> { self.range.map(|(s, e)| s..e) }

    pub fn src(&self) -> SrcId { self.src }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:", self.src)?;
        if let Some((a, b)) = self.range {
            write!(f, "{:?}", a..b)?;
        } else {
            write!(f, "?")?;
        }
        Ok(())
    }
}

impl chumsky::Span for Span {
    type Position = Option<usize>;

    fn start(&self) -> Self::Position { self.range.map(|(s, _)| s) }

    fn end(&self) -> Self::Position { self.range.map(|(_, e)| e) }

    fn union(self, other: Self) -> Self {
        assert_eq!(self.src, other.src, "attempted to union spans with different sources");
        Self {
            src: self.src,
            range: self.start()
                .zip_with(other.start(), usize::min)
                .zip(self.end()
                    .zip_with(other.end(), usize::max)),
        }
    }

    fn inner(self, other: Self) -> Self {
        assert_eq!(self.src, other.src, "attempted to find inner of spans with different sources");
        match self.end().zip(other.start()) {
            Some((e, s)) if e <= s => Self {
                src: self.src,
                range: Some((e, s)),
            },
            _ => panic!("spans intersect or are incorrectly ordered"),
        }
    }

    fn display(&self) -> Box<dyn fmt::Display + '_> {
        Box::new(format!("{:?}:{:?}..{:?}", self.src, self.start(), self.end()))
    }
}
