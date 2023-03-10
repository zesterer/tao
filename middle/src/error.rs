use super::*;
use std::io::Write;

#[derive(Debug)]
pub enum Error {
    NoEntryPoint(Span),
    MultipleEntryPoints(Span, Span),
    GenericEntryPoint(SrcNode<Ident>, Span, Span),
}

impl Error {
    pub fn write<C: ariadne::Cache<SrcId>>(self, _ctx: &Context, cache: C, writer: impl Write) {
        use ariadne::{Color, Fmt, Label, Report, ReportKind, Span};

        let (msg, spans, notes) = match self {
            Error::NoEntryPoint(root_span) => (
                format!("No main definition"),
                vec![(
                    root_span,
                    format!("Does not contain a definition marked as the main entry point"),
                    Color::Red,
                )],
                vec![format!(
                    "Mark a definition as the main entry point with {}",
                    "$[main]".fg(Color::Blue)
                )],
            ),
            Error::MultipleEntryPoints(a, b) => (
                format!("Multiple entry points"),
                vec![
                    (a, format!("First entry point is here"), Color::Red),
                    (b, format!("Second entry point is here"), Color::Red),
                ],
                vec![format!("A program may only have a single entry point")],
            ),
            Error::GenericEntryPoint(name, gen, entry) => (
                format!("Entry point {} cannot be generic", (*name).fg(Color::Red)),
                vec![
                    (gen, format!("Generics are not allowed here"), Color::Red),
                    (
                        entry,
                        format!("Declared as an entry point because of this attribute"),
                        Color::Yellow,
                    ),
                ],
                vec![format!("A program cannot be generic over types")],
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
