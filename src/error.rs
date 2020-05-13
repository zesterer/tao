use std::fmt;
use crate::{
    src::{SrcLoc, SrcRegion},
    lex::Token,
    node::SrcNode,
};

#[derive(Debug)]
pub struct Error {
    msg: String,
    regions: Vec<SrcRegion>,
    hints: Vec<String>,
}

impl Error {
    #[deprecated]
    pub fn custom(msg: String) -> Self {
        Self {
            msg,
            regions: Vec::new(),
            hints: Vec::new(),
        }
    }

    pub fn in_source<'a>(&'a self, src: &'a str) -> ErrorInSrc<'a> {
        ErrorInSrc {
            error: self,
            src,
        }
    }

    pub fn at(mut self, region: SrcRegion) -> Self {
        // TODO: More region information
        self
    }

    pub fn merge(mut self, mut other: Self) -> Self {
        // TODO: Merge errors
        self
    }

    pub fn with_region(mut self, region: SrcRegion) -> Self {
        self.regions.push(region);
        self
    }

    pub fn with_hint(mut self, hint: String) -> Self {
        self.hints.push(hint);
        self
    }
}

impl parze::error::Error<char> for Error {
    type Region = SrcRegion;
    type Thing = Thing;
    type Context = ();

    fn unexpected_sym(c: &char, region: SrcRegion) -> Self {
        Self::custom(format!("Unexpected character '{}'", c))
            .with_region(region)
    }

    fn unexpected_end() -> Self {
        Self::custom(format!("Unexpected end of input"))
    }

    fn expected_end(c: &char, region: SrcRegion) -> Self {
        Self::custom(format!("Expected end of input, found '{}'", c))
            .with_region(region)
    }

    fn expected(mut self, thing: Self::Thing) -> Self {
        // TODO: Merge error messages
        self
    }

    fn merge(self, other: Self) -> Self {
        self.merge(other)
    }
}

impl parze::error::Error<SrcNode<Token>> for Error {
    type Region = SrcRegion;
    type Thing = Thing;
    type Context = ();

    fn unexpected_sym(sym: &SrcNode<Token>, region: SrcRegion) -> Self {
        Self::custom(format!("Unexpected character '{}'", **sym))
            .with_region(sym.region())
            .with_region(region)
    }

    fn unexpected_end() -> Self {
        Self::custom(format!("Unexpected end of input"))
    }

    fn expected_end(sym: &SrcNode<Token>, region: SrcRegion) -> Self {
        Self::custom(format!("Expected end of input, found '{}'", **sym))
            .with_region(sym.region())
            .with_region(region)
    }

    fn expected(mut self, thing: Self::Thing) -> Self {
        // TODO: Merge error messages
        self
    }

    fn merge(self, other: Self) -> Self {
        self.merge(other)
    }
}

pub struct ErrorInSrc<'a> {
    error: &'a Error,
    src: &'a str,
}

impl<'a> fmt::Display for ErrorInSrc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let highlight_regions = |f: &mut fmt::Formatter, regions: &[_]| {
            let region_iter = regions
                .iter()
                .chain(self.error.regions.iter());

            if let Some(((start_line, start_col), (end_line, end_col))) = region_iter
                .clone()
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

                        writeln!(f, "{:>4} | {}", i + 1, line.replace("\t", " "))?;

                        // Underline
                        if region_iter
                            .clone()
                            .any(|r| r.intersects(line_region)) {
                            write!(f, "       ")?;
                            for _ in 0..line.len() {
                                if region_iter.clone().any(|r| r.contains(SrcLoc::at(char_pos))) {
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
            }

            Ok(())
        };

        write!(f, "Error: ")?;
        highlight_regions(f, &[])?;

        for hint in self.error.hints.iter() {
            writeln!(f, "Hint: {}", hint)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Thing {
    Char(char),
    Token(Token),
}

impl From<char> for Thing {
    fn from(c: char) -> Self {
        Thing::Char(c)
    }
}

impl From<Token> for Thing {
    fn from(token: Token) -> Self {
        Thing::Token(token)
    }
}

impl From<SrcNode<Token>> for Thing {
    fn from(token: SrcNode<Token>) -> Self {
        Self::from(token.into_inner())
    }
}

impl fmt::Display for Thing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Thing::Char(c) => write!(f, "'{}'", c),
            Thing::Token(t) => write!(f, "'{}'", t),
        }
    }
}
