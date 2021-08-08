use crate::{
    ast::{Loader, LoadCache, SrcId, Src},
    util::Span,
};
use ariadne::{
    Report, ReportKind,
    Cache,
    Label,
};
use std::fmt;

#[repr(u32)]
pub enum ErrorCode {
    NoSuchSrc = 0,
    ExpectedEnd = 1,
    UnexpectedEnd = 2,
    UnexpectedChar = 3,
    UnexpectedToken = 4,
    TypeMismatch = 5,
    TypeIncompatibility = 6,
    TypeInferenceFailure = 7,
    TypeInfinite = 8,
    SolverLimitReached = 9,
    InvalidUnaryOp = 10,
    InvalidBinaryOp = 11,
    DuplicateDef = 12,
}

impl ErrorCode {
    pub fn kind(&self) -> ReportKind {
        match self {
            ErrorCode::NoSuchSrc
            | ErrorCode::ExpectedEnd
            | ErrorCode::UnexpectedEnd
            | ErrorCode::UnexpectedChar
            | ErrorCode::UnexpectedToken
            | ErrorCode::TypeMismatch
            | ErrorCode::TypeIncompatibility
            | ErrorCode::TypeInferenceFailure
            | ErrorCode::TypeInfinite
            | ErrorCode::SolverLimitReached
            | ErrorCode::InvalidUnaryOp
            | ErrorCode::InvalidBinaryOp
            | ErrorCode::DuplicateDef => ReportKind::Error,
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
            ErrorCode::TypeIncompatibility => "E06",
            ErrorCode::TypeInferenceFailure => "E07",
            ErrorCode::TypeInfinite => "E08",
            ErrorCode::SolverLimitReached => "E09",
            ErrorCode::InvalidUnaryOp => "E10",
            ErrorCode::InvalidBinaryOp => "E11",
            ErrorCode::DuplicateDef => "E12",
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

    pub fn do_if(self, cond: bool, f: impl FnOnce(Self) -> Self) -> Self {
        if cond {
            f(self)
        } else {
            self
        }
    }

    pub fn emit(&self, cache: impl Cache<Span>) {
        Report::build(self.code.kind(), self.span, self.code as u32 as usize)
            .with_message(&self.msg)
            .with_label(Label::new(self.span)
                .with_message("Here"))
            .finish()
            .eprint(cache);
    }
}
