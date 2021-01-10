use crate::{
    ast::{Loader, LoadCache, SrcId, Src},
    util::Span,
};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, Severity, LabelStyle},
    files::Src as CodespanSrc,
    term::{self, termcolor::{ColorChoice, StandardStream}},
};
use std::fmt;

pub enum ErrorCode {
    NoSuchSrc,
    ExpectedEnd,
    UnexpectedEnd,
    UnexpectedChar,
    UnexpectedToken,
    TypeMismatch,
    CannotInferType,
}

impl ErrorCode {
    pub fn severity(&self) -> Severity {
        match self {
            ErrorCode::NoSuchSrc
            | ErrorCode::ExpectedEnd
            | ErrorCode::UnexpectedEnd
            | ErrorCode::UnexpectedChar
            | ErrorCode::UnexpectedToken
            | ErrorCode::TypeMismatch
            | ErrorCode::CannotInferType => Severity::Error,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ErrorCode::NoSuchSrc => "E00",
            ErrorCode::ExpectedEnd => "E01",
            ErrorCode::UnexpectedEnd => "E02",
            ErrorCode::UnexpectedChar => "E03",
            ErrorCode::UnexpectedToken => "E04",
            ErrorCode::TypeMismatch => "E05",
            ErrorCode::CannotInferType => "E06",
        })
    }
}

pub struct Error {
    pub code: ErrorCode,
    pub span: Span,
    pub msg: String,
    pub primary_spans: Vec<(Span, Option<String>)>,
    pub secondary_spans: Vec<(Span, Option<String>)>,
    pub notes: Vec<String>,
}

impl Error {
    pub fn new(code: ErrorCode, span: Span, msg: String) -> Self {
        Self {
            code,
            span,
            msg,
            primary_spans: Vec::new(),
            secondary_spans: Vec::new(),
            notes: Vec::new(),
        }
    }

    pub fn with_primary(mut self, span: Span, msg: Option<String>) -> Self {
        self.primary_spans.push((span, msg));
        self
    }

    pub fn with_secondary(mut self, span: Span, msg: Option<String>) -> Self {
        self.secondary_spans.push((span, msg));
        self
    }

    pub fn with_note(mut self, msg: String) -> Self {
        self.notes.push(msg);
        self
    }

    pub fn spans(&self) -> impl Iterator<Item=Span> + '_ {
        self.primary_spans
            .iter()
            .chain(self.secondary_spans
                .iter())
            .map(|(span, _)| *span)
    }

    pub fn emit(&self, load_cache: &mut LoadCache<impl Loader>) {
        let diag = Diagnostic::new(self.code.severity())
            .with_message(&self.msg)
            .with_code(format!("{}", self.code))
            .with_labels(
                self.primary_spans
                    .iter()
                    .zip(std::iter::repeat(LabelStyle::Primary))
                    .chain(self.secondary_spans
                        .iter()
                        .zip(std::iter::repeat(LabelStyle::Secondary)))
                    .filter_map(|((span, msg), style)| if let Some((src, range)) = span.src().zip(span.range()) {
                        Some(Label::new(style, src, range.start..range.end)
                            .with_message(msg.clone().unwrap_or_else(String::new)))
                    } else {
                        None
                    })
                    .collect()
            )
            .with_notes(self.notes.clone());

        impl CodespanSrc for Src {
            fn name(&self) -> &str { &self.name }
            fn source(&self) -> &str { &self.code }
        }

        term::emit(
            &mut StandardStream::stderr(ColorChoice::Always).lock(),
            &term::Config::default(),
            move |id| load_cache.load(id).unwrap_or_else(|_| panic!("ICE")).clone(),
            &diag,
        ).unwrap();
    }
}
