use super::*;
use std::collections::HashSet;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    UnexpectedEnd,
    Unexpected(Pattern),
    Unclosed { start: Pattern, before_span: Span, before: Option<Pattern> },
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
    expected: HashSet<Pattern>,
    label: Option<&'static str>,
}

impl Error {
    pub fn merge(mut self, other: Self) -> Self {
        // TODO: Use HashSet
        for expected in other.expected.into_iter() {
            self.expected.insert(expected);
        }
        self
    }

    pub fn print<C: ariadne::Cache<SrcId>>(self, cache: C) {
        use ariadne::{Report, ReportKind, Label, Color, Fmt};

        let msg = format!(
            "{}{}, expected {}",
            match &self.kind {
                ErrorKind::UnexpectedEnd => "Unexpected end of input".to_string(),
                ErrorKind::Unexpected(pat) => format!("Unexpected {}", pat.fg(Color::Red)),
                ErrorKind::Unclosed { start, .. } => format!("Unclosed delimiter {}", start.fg(Color::Red)),
            },
            if let Some(label) = self.label {
                format!(" while parsing {}", label)
            } else {
                "".to_string()
            },
            match self.expected.len() {
                0 => "end of input".to_string(),
                1 => format!("{}", self.expected.into_iter().next().unwrap().fg(Color::Cyan)),
                _ => format!("one of {}", self.expected.into_iter().map(|x| x.fg(Color::Cyan).to_string()).collect::<Vec<_>>().join(", ")),
            },
        );

        let report = Report::build(ReportKind::Error, self.span.src(), self.span.start())
            .with_code(3)
            .with_message(msg)
            .with_label(Label::new(self.span)
                .with_message(match &self.kind {
                    ErrorKind::UnexpectedEnd => "End of input".to_string(),
                    ErrorKind::Unexpected(pat) => format!("Unexpected {}", pat.fg(Color::Red)),
                    ErrorKind::Unclosed { start, .. } => format!("Delimiter {} is never closed", start.fg(Color::Red)),
                })
                .with_color(Color::Red));

        let report = if let ErrorKind::Unclosed { before, before_span, .. } = self.kind {
            report
                .with_label(Label::new(before_span)
                    .with_message(format!("Must be closed before {}", match before {
                        Some(before) => format!("this {}", before.fg(Color::Yellow)),
                        None => "end of input".to_string(),
                    }))
                .with_color(Color::Yellow))
        } else {
            report
        };

        report
            .finish()
            .print(cache)
            .unwrap();
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.span == other.span
            && self.label == other.label
    }
}

impl<T: Into<Pattern>> chumsky::Error<T> for Error {
    type Span = Span;
    type Label = &'static str;

    fn expected_input_found<Iter: IntoIterator<Item = T>>(
        span: Self::Span,
        expected: Iter,
        found: Option<T>,
    ) -> Self {
        Self {
            kind: found
                .map(Into::into)
                .map(ErrorKind::Unexpected)
                .unwrap_or(ErrorKind::UnexpectedEnd),
            span,
            expected: expected.into_iter().map(Into::into).collect(),
            label: None,
        }
    }

    fn unclosed_delimiter(
        span: Self::Span,
        start: T,
        before_span: Self::Span,
        expected: T,
        before: Option<T>,
    ) -> Self {
        Self {
            kind: ErrorKind::Unclosed {
                start: start.into(),
                before_span,
                before: before.map(Into::into),
            },
            span,
            expected: std::iter::once(expected.into()).collect(),
            label: None,
        }
    }

    fn with_label(mut self, label: Self::Label) -> Self {
        self.label.get_or_insert(label);
        self
    }

    fn merge(self, other: Self) -> Self {
        Error::merge(self, other)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Pattern {
    Char(char),
    Token(Token),
}

impl From<char> for Pattern { fn from(c: char) -> Self { Self::Char(c) } }
impl From<Token> for Pattern { fn from(tok: Token) -> Self { Self::Token(tok) } }

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Token(token) => write!(f, "{}", token),
            Pattern::Char(c) => write!(f, "{}", c),
        }
    }
}
