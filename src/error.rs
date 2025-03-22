use super::*;

#[derive(Debug)]
pub enum ErrorKind {
    DuplicateDef(Ident),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    labels: Vec<Span>,
}

impl Error {
    pub fn duplicate_def(name: Ident, old: Span, new: Span) -> Self {
        Self {
            kind: ErrorKind::DuplicateDef(name),
            labels: vec![old, new],
        }
    }
}

impl Error {
    pub fn emit(&self) {
        use ariadne::{Config, FnCache, IndexType, Label, Report, ReportKind};

        struct DiagSpan(Span);

        impl ariadne::Span for DiagSpan {
            type SourceId = Filename;

            fn source(&self) -> &Self::SourceId {
                &self.0.context
            }
            fn start(&self) -> usize {
                self.0.start()
            }
            fn end(&self) -> usize {
                self.0.end()
            }
        }

        Report::build(ReportKind::Error, DiagSpan(self.labels[0].clone()))
            .with_code(match &self.kind {
                ErrorKind::DuplicateDef(_) => 1,
            })
            .with_message(match &self.kind {
                ErrorKind::DuplicateDef(name) => {
                    format!("The definition {name} appears more than once")
                }
            })
            .with_labels({
                let labels = match &self.kind {
                    ErrorKind::DuplicateDef(_) => vec![
                        ("first instance", self.labels[0].clone()),
                        ("second instance", self.labels[1].clone()),
                    ],
                };

                labels
                    .into_iter()
                    .map(|(msg, span)| Label::new(DiagSpan(span)).with_message(msg))
            })
            .with_config(Config::default().with_index_type(IndexType::Byte))
            .finish()
            .eprint(FnCache::<_, _, String>::new(|s: &Filename| {
                std::fs::read_to_string(&*s.0)
            }))
            .unwrap();
    }
}
