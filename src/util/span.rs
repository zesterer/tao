use std::{ops::Range, cmp::Ordering, fmt};
use crate::ast::loader::SrcId;

#[derive(Copy, Clone)]
pub struct Span {
    src: Option<SrcId>,
    range: Option<(usize, usize)>,
}

impl Span {
    pub fn none() -> Self {
        Self {
            src: None,
            range: None,
        }
    }

    pub fn new(src: SrcId, range: Range<usize>) -> Self {
        Self {
            src: Some(src),
            range: Some((range.start, range.end)),
        }
    }

    pub fn with_src(mut self, src: SrcId) -> Self {
        self.src = Some(src);
        self
    }

    pub fn single(src: SrcId, pos: usize) -> Self {
        Span::new(src, pos..pos + 1)
    }

    pub fn src(&self) -> Option<SrcId> {
        self.src
    }

    pub fn range(&self) -> Option<Range<usize>> {
        self.range.map(|(a, b)| a..b)
    }

    pub fn union(self, other: Self) -> Self {
        assert!(self.src.zip_with(other.src, |a, b| a == b).unwrap_or(true));
        Self {
            src: self.src,
            range: self.range
                .zip_with(other.range, |(a, b), (c, d)| (a.min(c), b.max(d)))
                .or(self.range)
                .or(other.range),
        }
    }

    pub fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.src == other.src {
            match (&self.range, &other.range) {
                (Some(a), Some(b)) => a.0.partial_cmp(&b.0),
                (None, None) => Some(Ordering::Equal),
                (None, _) => Some(Ordering::Greater),
                (_, None) => Some(Ordering::Less),
            }
        } else {
            None
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.range {
            Some((a, b)) => write!(f, "{}..{}", a, b),
            None => write!(f, "<?>"),
        }
    }
}
