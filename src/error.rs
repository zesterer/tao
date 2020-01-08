use std::{
    fmt,
    collections::HashSet,
};
use crate::{
    src::{SrcLoc, SrcRegion},
    lex::Token,
    parse::{UnaryOp, BinaryOp},
    node::Node,
};

pub struct Error {
    kind: ErrorKind,
    region: SrcRegion,
}

impl Error {
    pub fn invalid_unary_op(op: UnaryOp, region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::InvalidUnaryOp(op),
            region,
        }
    }

    pub fn invalid_binary_op(op: BinaryOp, region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::InvalidBinaryOp(op),
            region,
        }
    }

    pub fn cannot_call(region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::CannotCall,
            region,
        }
    }

    pub fn not_truthy(region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::NotTruthy,
            region,
        }
    }

    pub fn no_such_binding(name: String, region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::NoSuchBinding(name),
            region,
        }
    }

    pub fn in_source<'a>(&'a self, src: &'a str) -> ErrorInSrc<'a> {
        ErrorInSrc {
            error: self,
            src,
        }
    }

    pub fn merge(mut self, mut other: Self) -> Self {
        // TODO
        self
    }
}

impl parze::error::Error<char> for Error {
    type Context = ();

    fn unexpected_sym(sym: char, at: parze::Index) -> Self {
        Self {
            kind: ErrorKind::FoundExpected(Thing::Char(sym), HashSet::new()),
            region: SrcRegion::single(SrcLoc::at(at as usize)),
        }
    }

    fn unexpected_end() -> Self {
        Self {
            kind: ErrorKind::UnexpectedEnd,
            region: SrcRegion::none(),
        }
    }

    fn expected_end(sym: char, at: parze::Index) -> Self {
        Self {
            kind: ErrorKind::ExpectedEnd(Thing::Char(sym)),
            region: SrcRegion::single(SrcLoc::at(at as usize)),
        }
    }

    fn expected(mut self, sym: char) -> Self {
        match &mut self.kind {
            ErrorKind::FoundExpected(_, expected) => { expected.insert(Thing::Char(sym)); },
            _ => {},
        }
        self
    }

    fn merge(self, other: Self) -> Self {
        self.merge(other)
    }
}

impl parze::error::Error<Node<Token>> for Error {
    type Context = ();

    fn unexpected_sym(sym: Node<Token>, at: parze::Index) -> Self {
        Self {
            region: sym.region(),
            kind: ErrorKind::FoundExpected(Thing::Token(sym.into_inner()), HashSet::new()),
        }
    }

    fn unexpected_end() -> Self {
        Self {
            kind: ErrorKind::UnexpectedEnd,
            region: SrcRegion::none(),
        }
    }

    fn expected_end(sym: Node<Token>, at: parze::Index) -> Self {
        Self {
            region: sym.region(),
            kind: ErrorKind::ExpectedEnd(Thing::Token(sym.into_inner())),
        }
    }

    fn expected(mut self, sym: Node<Token>) -> Self {
        match &mut self.kind {
            ErrorKind::FoundExpected(_, expected) => { expected.insert(Thing::Token(sym.into_inner())); },
            _ => {},
        }
        self
    }

    fn merge(self, _other: Self) -> Self {
        self
    }
}

pub enum ErrorKind {
    FoundExpected(Thing, HashSet<Thing>),
    UnexpectedEnd,
    ExpectedEnd(Thing),
    InvalidUnaryOp(UnaryOp),
    InvalidBinaryOp(BinaryOp),
    CannotCall,
    NotTruthy,
    NoSuchBinding(String),
}

pub struct ErrorInSrc<'a> {
    error: &'a Error,
    src: &'a str,
}

impl<'a> fmt::Display for ErrorInSrc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let error_pos = self.error.region.in_context(self.src);

        match self.error.kind {
            _ => write!(f, "Error")?,
            // "Warning"
        };
        write!(f, " at ")?;
        match error_pos {
            Some(((line, col), _)) => write!(f, "line {}, column {}", line + 1, col + 1)?,
            None => write!(f, "<unknown>")?,
        };
        write!(f, ": ")?;
        match &self.error.kind {
            ErrorKind::FoundExpected(found, expected) => {
                let expected = expected.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(", ");
                write!(f, "Found {}, expected {}", found, expected)?
            },
            ErrorKind::UnexpectedEnd => write!(f, "Unexpected end of input")?,
            ErrorKind::ExpectedEnd(found) => write!(f, "Expected end of input, found {}", found)?,
            ErrorKind::InvalidUnaryOp(op) => write!(f, "Invalid unary operation {:?}", op)?,
            ErrorKind::InvalidBinaryOp(op) => write!(f, "Invalid binary operation {:?}", op)?,
            ErrorKind::CannotCall => write!(f, "Cannot call value")?,
            ErrorKind::NotTruthy => write!(f, "Cannot determine truth of value")?,
            ErrorKind::NoSuchBinding(name) => write!(f, "Cannot find binding '{}'", name)?,
        };
        writeln!(f, "")?;

        if let Some(((start_line, start_col), (end_line, end_col))) = error_pos {
            let draw_underline = |f: &mut fmt::Formatter, line_len, start, end| {
                for _ in 0..start {
                    write!(f, " ")?;
                }
                for _ in start..end {
                    write!(f, "^")?;
                }
                for _ in end..line_len {
                    write!(f, " ")?;
                }
                Ok(())
            };

            let lines = self.src.lines().collect::<Vec<_>>();
            if start_line == end_line {
                writeln!(f, "{:>4} | {}", start_line + 1, lines[start_line])?;
                write!(f, "       ")?;
                draw_underline(f, lines[start_line].len(), start_col, end_col)?;
                writeln!(f, "")?;
            } else {
                todo!()
            }
        } else {
            todo!()
        }

        Ok(())
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Thing {
    Char(char),
    Token(Token),
}

impl fmt::Display for Thing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Thing::Char(c) => write!(f, "'{}'", c),
            Thing::Token(t) => write!(f, "{:?}", t),
        }
    }
}
