use internment::Intern;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SrcId(Intern<Vec<String>>);

impl SrcId {
    #[cfg(test)]
    pub fn empty() -> Self {
        SrcId(Intern::new(Vec::new()))
    }
}

impl fmt::Debug for SrcId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.len() == 0 {
            write!(f, "?")
        } else {
            write!(f, "{}", self.0.clone().join("/"))
        }
    }
}
