use std::{
    fmt,
    collections::HashSet,
};
use crate::{
    src::{SrcLoc, SrcRegion},
    lex::Token,
    parse::{UnaryOp, BinaryOp},
    hir::TypeInfo,
    node::Node,
};

pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn invalid_unary_op(op: Node<UnaryOp>, a: Node<TypeInfo>) -> Self {
        Self {
            kind: ErrorKind::InvalidUnaryOp(op, a),
        }
    }

    pub fn invalid_binary_op(op: Node<BinaryOp>, a: Node<TypeInfo>, b: Node<TypeInfo>) -> Self {
        Self {
            kind: ErrorKind::InvalidBinaryOp(op, a, b),
        }
    }

    pub fn cannot_call(region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::CannotCall(region),
        }
    }

    pub fn not_truthy(region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::NotTruthy(region),
        }
    }

    pub fn no_such_binding(name: String, region: SrcRegion) -> Self {
        Self {
            kind: ErrorKind::NoSuchBinding(name, region),
        }
    }

    pub fn type_mismatch(a: Node<TypeInfo>, b: Node<TypeInfo>) -> Self {
        Self {
            kind: ErrorKind::TypeMismatch(a, b),
        }
    }

    pub fn cannot_infer_type(a: Node<TypeInfo>) -> Self {
        Self {
            kind: ErrorKind::CannotInferType(a),
        }
    }

    pub fn recursive_type(a: Node<TypeInfo>) -> Self {
        Self {
            kind: ErrorKind::RecursiveType(a),
        }
    }

    pub fn in_source<'a>(&'a self, src: &'a str) -> ErrorInSrc<'a> {
        ErrorInSrc {
            error: self,
            src,
        }
    }

    pub fn merge(self, other: Self) -> Self {
        // TODO
        self
    }
}

impl parze::error::Error<char> for Error {
    type Context = ();

    fn unexpected_sym(sym: char, at: parze::Index) -> Self {
        Self {
            kind: ErrorKind::FoundExpected(
                Thing::Char(sym),
                SrcRegion::single(SrcLoc::at(at as usize)),
                HashSet::new(),
            ),
        }
    }

    fn unexpected_end() -> Self {
        Self {
            kind: ErrorKind::UnexpectedEnd,
        }
    }

    fn expected_end(sym: char, at: parze::Index) -> Self {
        Self {
            kind: ErrorKind::ExpectedEnd(
                Thing::Char(sym),
                SrcRegion::single(SrcLoc::at(at as usize)),
            ),
        }
    }

    fn expected(mut self, sym: char) -> Self {
        match &mut self.kind {
            ErrorKind::FoundExpected(_, _, expected) => { expected.insert(Thing::Char(sym)); },
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
            kind: ErrorKind::FoundExpected(
                Thing::Token(sym.into_inner()),
                SrcRegion::single(SrcLoc::at(at as usize)),
                HashSet::new(),
            ),
        }
    }

    fn unexpected_end() -> Self {
        Self {
            kind: ErrorKind::UnexpectedEnd,
        }
    }

    fn expected_end(sym: Node<Token>, at: parze::Index) -> Self {
        let region = sym.region;
        Self {
            kind: ErrorKind::ExpectedEnd(
                Thing::Token(sym.into_inner()),
                region,
            ),
        }
    }

    fn expected(mut self, sym: Node<Token>) -> Self {
        match &mut self.kind {
            ErrorKind::FoundExpected(_, _, expected) => { expected.insert(Thing::Token(sym.into_inner())); },
            _ => {},
        }
        self
    }

    fn merge(self, _other: Self) -> Self {
        self
    }
}

pub enum ErrorKind {
    FoundExpected(Thing, SrcRegion, HashSet<Thing>),
    UnexpectedEnd,
    ExpectedEnd(Thing, SrcRegion),
    InvalidUnaryOp(Node<UnaryOp>, Node<TypeInfo>),
    InvalidBinaryOp(Node<BinaryOp>, Node<TypeInfo>, Node<TypeInfo>),
    CannotCall(SrcRegion),
    NotTruthy(SrcRegion),
    NoSuchBinding(String, SrcRegion),
    TypeMismatch(Node<TypeInfo>, Node<TypeInfo>),
    CannotInferType(Node<TypeInfo>),
    RecursiveType(Node<TypeInfo>),
}

pub struct ErrorInSrc<'a> {
    error: &'a Error,
    src: &'a str,
}

impl<'a> fmt::Display for ErrorInSrc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let highlight_regions = |f: &mut fmt::Formatter, regions: &[_]| {
            if let Some(((start_line, start_col), (end_line, end_col))) = regions
                .iter()
                .fold(SrcRegion::none(), |a, x| a.union(*x))
                .in_context(self.src)
            {
                writeln!(f, "-> line {}, column {}", start_line + 1, start_col + 1)?;

                let lines = self.src.lines().collect::<Vec<_>>();

                let mut char_pos = 0;
                for (i, line) in lines.iter().enumerate() {
                    if i >= start_line && i <= end_line {
                        let line_region = SrcRegion::range(
                            SrcLoc::at(char_pos),
                            SrcLoc::at(char_pos + line.len()),
                        );

                        writeln!(f, "{:>4} | {}", i + 1, line)?;

                        // Underline
                        if regions.iter().any(|r| r.intersects(line_region)) {
                            write!(f, "       ")?;
                            for _ in 0..line.len() {
                                if regions.iter().any(|r| r.contains(SrcLoc::at(char_pos))) {
                                    write!(f, "^")?;
                                } else {
                                    write!(f, " ")?;
                                }

                                char_pos += 1;
                            }
                            writeln!(f, "")?;
                            char_pos += 1;
                        } else {
                            char_pos += line.len() + 1;
                        }
                    } else {
                        char_pos += line.len() + 1;
                    }
                }
            } else {
                todo!()
            }

            Ok(())
        };

        match self.error.kind {
            _ => write!(f, "Error")?,
            // "Warning"
        };
        write!(f, ": ")?;
        match &self.error.kind {
            ErrorKind::FoundExpected(found, region, expected) => {
                let expected = expected.iter().map(|e| format!("{}", e)).collect::<Vec<_>>().join(", ");
                writeln!(f, "Found {}, expected {}", found, expected)?;
                highlight_regions(f, &[*region])?;
            },
            ErrorKind::UnexpectedEnd => {
                writeln!(f, "Unexpected end of input")?;
            },
            ErrorKind::ExpectedEnd(found, region) => {
                writeln!(f, "Expected end of input, found {}", found)?;
                highlight_regions(f, &[*region])?;
            },
            ErrorKind::InvalidUnaryOp(op, a) => {
                writeln!(f, "Cannot apply '{}' to value of type '{}'", op.inner, a.inner)?;
                highlight_regions(f, &[op.region, a.region])?;
            },
            ErrorKind::InvalidBinaryOp(op, a, b) => {
                writeln!(f, "Cannot apply '{}' to values of types '{}' and '{}'", op.inner, a.inner, b.inner)?;
                highlight_regions(f, &[op.region, a.region, b.region])?;
            },
            ErrorKind::CannotCall(region) => {
                writeln!(f, "Value does not support function application")?;
                highlight_regions(f, &[*region])?;
            },
            ErrorKind::NotTruthy(region) => {
                writeln!(f, "Cannot determine truth of value")?;
                highlight_regions(f, &[*region])?;
            },
            ErrorKind::NoSuchBinding(name, region) => {
                writeln!(f, "No such binding '{}' in the current scope", name)?;
                highlight_regions(f, &[*region])?;
            },
            ErrorKind::TypeMismatch(a, b) => {
                if a.region.intersects(b.region) {
                    writeln!(f, "Conflicting requirements imply value to be of types '{}' and '{}' at once", a.inner(), b.inner())?;
                } else {
                    writeln!(f, "Type mismatch between '{}' and '{}'", a.inner(), b.inner())?;
                }
                highlight_regions(f, &[a.region, b.region])?;
            },
            ErrorKind::CannotInferType(ty) => {
                writeln!(f, "Insufficient information to fully infer type '{}'", ty.inner)?;
                highlight_regions(f, &[ty.region])?;
            },
            ErrorKind::RecursiveType(ty) => {
                writeln!(f, "Recursive type detected '{}'", ty.inner)?;
                highlight_regions(f, &[ty.region])?;
            },
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
