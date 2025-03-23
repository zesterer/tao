use super::*;

#[derive(Debug)]
pub enum ErrorKind {
    DuplicateDef(Ident),
    ExpectedFound(Vec<String>, String),
    UnresolvedLocal(Ident),
    CyclicDependency(PkgId, Vec<PkgId>),
    MissingManifestKey(Ident),
    NoMain,
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

    pub fn unresolved_local(local: Ident, span: Span) -> Self {
        Self {
            kind: ErrorKind::UnresolvedLocal(local),
            labels: vec![span],
        }
    }

    pub fn cyclic_dependency(pkg: PkgId, manifest_span: Span, cycle: Vec<PkgId>) -> Self {
        Self {
            kind: ErrorKind::CyclicDependency(pkg, cycle),
            labels: vec![manifest_span],
        }
    }

    pub fn missing_manifest_key(key: Ident, span: Span) -> Self {
        Self {
            kind: ErrorKind::MissingManifestKey(key),
            labels: vec![span],
        }
    }

    pub fn no_main(span: Span) -> Self {
        Self {
            kind: ErrorKind::NoMain,
            labels: vec![span],
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
                ErrorKind::DuplicateDef(..) => 1,
                ErrorKind::ExpectedFound(..) => 2,
                ErrorKind::UnresolvedLocal(..) => 3,
                ErrorKind::CyclicDependency(..) => 4,
                ErrorKind::MissingManifestKey(..) => 5,
                ErrorKind::NoMain => 6,
            })
            .with_message(match &self.kind {
                ErrorKind::DuplicateDef(name) => {
                    format!("The definition {name} appears more than once")
                }
                ErrorKind::ExpectedFound(expected, found) => {
                    format!("Found `{found}`, but expected {}", expected.join(", "))
                }
                ErrorKind::UnresolvedLocal(local) => {
                    format!("Local `{local}` was not found in scope")
                }
                ErrorKind::CyclicDependency(pkg, cycle) => {
                    format!(
                        "Package `{pkg}` is cyclically-dependent (chain is {})",
                        cycle
                            .iter()
                            .map(|p| format!("`{p}`"))
                            .collect::<Vec<_>>()
                            .join(" -> ")
                    )
                }
                ErrorKind::MissingManifestKey(key) => {
                    format!("Manifest key `{key}` is required in this node")
                }
                ErrorKind::NoMain => {
                    format!("Root package does not contain a `main` def")
                }
            })
            .with_labels({
                let labels = match &self.kind {
                    ErrorKind::DuplicateDef(_) => vec![
                        (format!("first instance"), self.labels[0].clone()),
                        (format!("second instance"), self.labels[1].clone()),
                    ],
                    ErrorKind::ExpectedFound(_, found) => {
                        vec![(format!("unexpected `{found}`"), self.labels[0].clone())]
                    }
                    ErrorKind::UnresolvedLocal(..) => {
                        vec![(format!("not found"), self.labels[0].clone())]
                    }
                    ErrorKind::CyclicDependency(..) => {
                        vec![(
                            format!("this package is cyclically dependent"),
                            self.labels[0].clone(),
                        )]
                    }
                    ErrorKind::MissingManifestKey(key) => {
                        vec![(
                            format!("manifest key `{key}` should be present in this node"),
                            self.labels[0].clone(),
                        )]
                    }
                    ErrorKind::NoMain => {
                        vec![(
                            format!("does not contain a `main` def"),
                            self.labels[0].clone(),
                        )]
                    }
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

impl<'src> From<chumsky::error::Rich<'src, char, Span>> for Error {
    fn from(rich: chumsky::error::Rich<'src, char, Span>) -> Self {
        Self {
            kind: match rich.reason() {
                chumsky::error::RichReason::ExpectedFound { expected, found } => {
                    ErrorKind::ExpectedFound(
                        expected.into_iter().map(|e| e.to_string()).collect(),
                        found
                            .map(|e| e.to_string())
                            .unwrap_or_else(|| "end of input".to_string()),
                    )
                }
                chumsky::error::RichReason::Custom(_) => todo!(),
            },
            labels: vec![rich.span().clone()],
        }
    }
}

impl<'src> From<chumsky::error::Rich<'src, syntax::TokenTree, Span>> for Error {
    fn from(rich: chumsky::error::Rich<'src, syntax::TokenTree, Span>) -> Self {
        Self {
            kind: match rich.reason() {
                chumsky::error::RichReason::ExpectedFound { expected, found } => {
                    ErrorKind::ExpectedFound(
                        expected.into_iter().map(|e| format!("{e:?}")).collect(),
                        found
                            .as_ref()
                            .map(|e| format!("{e:?}"))
                            .unwrap_or_else(|| "end of input".to_string()),
                    )
                }
                chumsky::error::RichReason::Custom(_) => todo!(),
            },
            labels: vec![rich.span().clone()],
        }
    }
}
