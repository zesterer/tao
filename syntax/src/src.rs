use internment::Intern;
use std::{
    path::Path,
    fmt,
};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SrcId(Intern<Vec<String>>);

impl fmt::Display for SrcId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.len() == 0 {
            write!(f, "?")
        } else {
            write!(f, "{}", self.0.clone().join("/"))
        }
    }
}

impl fmt::Debug for SrcId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self) }
}

impl SrcId {
    #[cfg(test)]
    pub fn empty() -> Self {
        SrcId(Intern::new(Vec::new()))
    }

    pub fn repl() -> Self {
        SrcId(Intern::new(vec!["repl".to_string()]))
    }

    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        SrcId(Intern::new(path
            .as_ref()
            .iter()
            .map(|c| c.to_string_lossy().into_owned())
            .collect()))
    }
}
