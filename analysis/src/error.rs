use super::*;

#[derive(Debug)]
pub enum Error {
    Mismatch(TyId, TyId),
    CannotInfer(TyId),
    NoSuchField(TyId, SrcNode<Ident>),
    NoSuchLocal(SrcNode<Ident>),
    WrongNumberOfParams(Span, usize, Span, usize),
    NoBranches(Span),
    InvalidUnaryOp(SrcNode<ast::UnaryOp>, TyId, Span),
    InvalidBinaryOp(SrcNode<ast::BinaryOp>, TyId, Span, TyId, Span),
    NoSuchData(SrcNode<Ident>),
    NoSuchCons(SrcNode<Ident>),
    RecursiveAlias(AliasId, Span),
}

impl Error {
    pub fn print<C: ariadne::Cache<SrcId>>(self, ctx: &Context, cache: C) {
        use ariadne::{Report, ReportKind, Label, Color, Fmt, Span};

        let display = |id| ctx.tys.display(&ctx.datas, id);

        let (msg, spans) = match self {
            Error::Mismatch(a, b) => (
                format!("Type mismatch between {} and {}", display(a).fg(Color::Red), display(b).fg(Color::Red)),
                vec![ctx.tys.get_span(a), ctx.tys.get_span(b)],
            ),
            Error::CannotInfer(a) => (format!("Cannot infer type {}", display(a).fg(Color::Red)), vec![ctx.tys.get_span(a)]),
            Error::NoSuchField(a, field) => (
                format!("No such field {} on {}", (*field).fg(Color::Red), display(a).fg(Color::Red)),
                vec![ctx.tys.get_span(a), field.span()],
            ),
            Error::NoSuchLocal(local) => (
                format!("No such local {}", (*local).fg(Color::Red)),
                vec![local.span()],
            ),
            Error::WrongNumberOfParams(a, a_count, b, b_count) => (
                format!("Pattern arms have different numbers of parameters"),
                vec![a, b],
            ),
            Error::NoBranches(span) => (
                format!("Pattern match must have at least one branch"),
                vec![span],
            ),
            Error::InvalidUnaryOp(op, a, a_span) => (
                format!("Cannot resolve {} {}", (*op).fg(Color::Red), display(a).fg(Color::Red)),
                vec![a_span],
            ),
            Error::InvalidBinaryOp(op, a, a_span, b, b_span) => (
                format!("Cannot resolve {} {} {}", display(a).fg(Color::Red), (*op).fg(Color::Red), display(b).fg(Color::Red)),
                vec![a_span, b_span],
            ),
            Error::NoSuchData(a) => (format!("No such type {}", (*a).fg(Color::Red)), vec![a.span()]),
            Error::NoSuchCons(a) => (format!("No such constructor {}", (*a).fg(Color::Red)), vec![a.span()]),
            Error::RecursiveAlias(alias, span) => (
                format!("Recursive type alias detected"),
                vec![ctx.datas.get_alias_span(alias), span],
            ),
        };

        let mut report = Report::build(ReportKind::Error, spans.first().unwrap().src(), spans.first().unwrap().start())
            .with_code(3)
            .with_message(msg);

        for span in spans {
            report = report.with_label(Label::new(span)
                .with_message(format!("TODO"))
                .with_color(Color::Red));
        }

        report
            .finish()
            .print(cache)
            .unwrap();
    }
}
