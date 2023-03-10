use super::*;
use std::io::Write;

#[derive(Debug)]
pub enum Error {
    CannotImport(SrcNode<Intern<String>>),
}

impl Error {
    pub fn write<C: ariadne::Cache<SrcId>>(self, cache: C, writer: impl Write) {
        use ariadne::{Color, Fmt, Label, Report, ReportKind, Span};

        let (msg, spans, notes) = match self {
            Error::CannotImport(path) => (
                format!("Cannot import {}, no such file", (*path).fg(Color::Red)),
                vec![(path.span(), "Does not exist".to_string(), Color::Red)],
                vec![format!("The file {} must exist", (*path).fg(Color::Yellow))],
            ),
        };

        let mut report = Report::build(
            ReportKind::Error,
            spans.first().unwrap().0.src(),
            spans.first().unwrap().0.start(),
        )
        .with_code(3)
        .with_message(msg);

        for (span, msg, col) in spans {
            report = report.with_label(Label::new(span).with_message(msg).with_color(col));
        }

        for note in notes {
            report = report.with_note(note);
        }

        report.finish().write(cache, writer).unwrap();
    }
}
