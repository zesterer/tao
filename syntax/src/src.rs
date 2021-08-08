use internment::Intern;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SrcId(Intern<Vec<String>>);

impl SrcId {
    #[cfg(test)]
    pub fn empty() -> Self {
        SrcId(Intern::new(Vec::new()))
    }
}
