use super::*;
use std::io::Write;

#[derive(Debug)]
pub enum Error {
    // Outer type, inner type
    CannotCoerce(TyId, TyId, Option<(TyId, TyId)>, EqInfo),
    CannotInfer(TyId, Option<Span>),
    Recursive(TyId, Span, Span),
    NoSuchItem(TyId, Span, SrcNode<Ident>),
    NoSuchField(TyId, Span, SrcNode<Ident>),
    NoSuchLocal(SrcNode<Ident>),
    WrongNumberOfParams(Span, usize, Span, usize),
    NoBranches(Span),
    InvalidUnaryOp(SrcNode<ast::UnaryOp>, TyId, Span),
    InvalidBinaryOp(SrcNode<ast::BinaryOp>, TyId, Span, TyId, Span),
    // (obligation, type, obligation_origin, generic_definition
    TypeDoesNotFulfil(ClassId, TyId, Span, Option<Span>),
    NoSuchData(SrcNode<Ident>),
    NoSuchCons(SrcNode<Ident>),
    NoSuchClass(SrcNode<Ident>),
    NoSuchClassItem(SrcNode<Ident>, SrcNode<Ident>),
    AmbiguousClassItem(SrcNode<Ident>, Vec<ClassId>),
    MissingClassItem(Span, SrcNode<Ident>, SrcNode<Ident>),
    RecursiveAlias(AliasId, TyId, Span),
    DuplicateTypeName(Ident, Span, Span),
    DuplicateDefName(Ident, Span, Span),
    DuplicateConsName(Ident, Span, Span),
    DuplicateGenName(Ident, Span, Span),
    DuplicateClassName(Ident, Span, Span),
    DuplicateClassItem(Ident, Span, Span),
    DuplicateMemberItem(Ident, Span, Span),
    PatternNotSupported(TyId, SrcNode<ast::BinaryOp>, TyId, Span),
    // Span, uncovered example, hidden_outer
    NotExhaustive(Span, ExamplePat, bool),
    WrongNumberOfGenerics(Span, usize, Span, usize),
    DefTypeNotSpecified(Span, Span, Ident),
    SelfNotValidHere(Span),
    NoEntryPoint(Span),
    MultipleEntryPoints(Span, Span),
    GenericEntryPoint(SrcNode<Ident>, Span),
    InvalidIntrinsic(SrcNode<Ident>),
    Unsupported(Span, &'static str),
    NonNumeric(TyId, Span, NumLitr),
    MissingLangItem(&'static str),
}

impl Error {
    pub fn write<C: ariadne::Cache<SrcId>>(self, ctx: &Context, cache: C, main_src: SrcId, writer: impl Write) {
        use ariadne::{Report, ReportKind, Label, Color, Fmt, Span, Config};

        let display = |id| ctx.tys.display(&ctx.datas, id);

        let (msg, spans, notes) = match self {
            Error::CannotCoerce(x, y, inner, info) => {
                let src = inner.map_or(x, |(a, _)| a);
                let dst = inner.map_or(y, |(_, b)| b);
                (
                    format!(
                        "Type {} does not coerce to {}",
                        display(src).fg(Color::Red),
                        display(dst).fg(Color::Yellow),
                    ),
                    {
                        let mut labels = vec![
                            (ctx.tys.get_span(src), format!("Type {} was found here", display(src).fg(Color::Red)), Color::Red),
                            (ctx.tys.get_span(dst), format!("Type {} is required here", display(dst).fg(Color::Yellow)), Color::Yellow),
                        ];
                        if let Some(at) = info.at {
                            labels.push((at, format!("Coercion is required here"), Color::Yellow));
                        }
                        labels
                    },
                    if let Some(reason) = info.reason {
                        vec![reason]
                    } else {
                        Vec::new()
                    },
                )
            },
            Error::CannotInfer(a, origin) => (
                format!("Cannot infer type {}", display(a).fg(Color::Red)),
                match origin {
                    Some(origin) => vec![
                        (ctx.tys.get_span(a), format!("Use of generic item"), Color::Red),
                        (origin, format!("Instantiation of this generic type"), Color::Red)
                    ],
                    None => vec![(ctx.tys.get_span(a), format!("{}", display(a)), Color::Red)],
                },
                vec![],
            ),
            Error::Recursive(a, span, part) => (
                format!("Self-referencing type {} expands to have infinite size", display(a).fg(Color::Red)),
                vec![
                    (span, format!("Mentions itself"), Color::Red),
                    (part, format!("Self-reference occurs here"), Color::Yellow),
                ],
                vec![
                    format!("Types expand eagerly and so self-reference results in infinite size"),
                    format!("If this was intentional, consider using {} instead", "data".fg(Color::Cyan)),
                ],
            ),
            Error::NoSuchItem(a, record_span, field) => (
                format!("Type {} has no item named {}", display(a).fg(Color::Red), (*field).fg(Color::Red)),
                vec![
                    (record_span, format!("Has type {}", display(a).fg(Color::Yellow)), Color::Yellow),
                    (field.span(), format!("Item does not exist"), Color::Red),
                ],
                vec![],
            ),
            Error::NoSuchField(a, record_span, field) => (
                format!("Type {} has no field named {}", display(a).fg(Color::Red), (*field).fg(Color::Red)),
                vec![
                    (record_span, format!("Has type {}", display(a).fg(Color::Yellow)), Color::Yellow),
                    (field.span(), format!("Field does not exist"), Color::Red),
                ],
                vec![],
            ),
            Error::NoSuchLocal(local) => (
                format!("No such local {}", (*local).fg(Color::Red)),
                vec![(local.span(), format!("Scope does not contain this"), Color::Red)],
                vec![],
            ),
            Error::WrongNumberOfParams(a, a_count, b, b_count) => (
                format!("Pattern arms must all have the same number of parameters"),
                vec![
                    (a, format!("Has {} parameter(s)", a_count), Color::Red),
                    (b, format!("Has {} parameter(s)", b_count), Color::Red),
                ],
                vec![],
            ),
            Error::NoBranches(span) => (
                format!("Pattern match must have at least one branch"),
                vec![(span, format!("Must have a branch"), Color::Red)],
                vec![],
            ),
            Error::InvalidUnaryOp(op, a, a_span) => (
                format!("Cannot apply {} to {}", (*op).fg(Color::Red), display(a).fg(Color::Red)),
                vec![
                    (op.span(), format!("Operation {} applied here", (*op).fg(Color::Red)), Color::Red),
                    (a_span, format!("{}", display(a).fg(Color::Yellow)), Color::Yellow),
                ],
                match ctx.tys.get(a) {
                    Ty::Gen(_, _) => vec![format!(
                        "Consider adding a class constraint like {}",
                        format!("{} < {:?}", display(a), *op).fg(Color::Blue),
                    )],
                    _ => vec![],
                },
            ),
            Error::InvalidBinaryOp(op, a, a_span, b, b_span) => (
                format!("Invalid operation {} {} {}", display(a).fg(Color::Red), (*op).fg(Color::Red), display(b).fg(Color::Red)),
                vec![
                    (op.span(), format!("Operation {} applied here", (*op).fg(Color::Red)), Color::Red),
                    (a_span, format!("{}", display(a).fg(Color::Yellow)), Color::Yellow),
                    (b_span, format!("{}", display(b).fg(Color::Yellow)), Color::Yellow),
                ],
                match ctx.tys.get(a) {
                    Ty::Gen(_, _) => vec![format!(
                        "Consider adding a class constraint like {}",
                        format!("{} < {:?} {}", display(a), *op, display(b)).fg(Color::Blue),
                    )],
                    _ => vec![],
                },
            ),
            Error::TypeDoesNotFulfil(class, ty, span, gen_span) => (
                format!("Type {} does not fulfil {} obligation", display(ty).fg(Color::Red), (*ctx.classes.get(class).name).fg(Color::Red)),
                {
                    let mut labels = vec![
                        (span, format!("Obligation is required here"), Color::Yellow),
                        (ctx.tys.get_span(ty), format!(
                            "{} must be a member of {}",
                            display(ty).fg(Color::Red),
                            (*ctx.classes.get(class).name).fg(Color::Red),
                        ), Color::Red),
                    ];
                    if let Some(gen_span) = gen_span {
                        labels.push((gen_span, format!(
                            "Consider adding a class constraint like {}",
                            format!("{} < {}", display(ty), *ctx.classes.get(class).name).fg(Color::Blue),
                        ), Color::Blue));
                    }
                    labels
                },
                vec![format!("Types must fulfil their class obligations")],
            ),
            Error::NoSuchData(a) => (
                format!("No such type {}", (*a).fg(Color::Red)),
                vec![(a.span(), format!("Does not exist"), Color::Red)],
                vec![],
            ),
            Error::NoSuchCons(a) => (
                format!("No such constructor {}", (*a).fg(Color::Red)),
                vec![(a.span(), format!("Does not exist"), Color::Red)],
                vec![],
            ),
            Error::NoSuchClass(a) => (
                format!("No such class {}", (*a).fg(Color::Red)),
                vec![(a.span(), format!("Does not exist"), Color::Red)],
                vec![],
            ),
            Error::NoSuchClassItem(item, class) => (
                format!("No such item {} on class {}", (*item).fg(Color::Red), (*class).fg(Color::Red)),
                vec![
                    (item.span(), format!("This item is not required in the class contract"), Color::Red),
                    (class.span(), format!("Does not have an item named {}", (*item).fg(Color::Red)), Color::Yellow),
                ],
                vec![format!("Class members must provide only the items required by their class")],
            ),
            Error::MissingClassItem(member, class, item) => (
                format!("Member of class {} is missing class item {}", (*class).fg(Color::Red), (*item).fg(Color::Red)),
                vec![
                    (member, format!("This member does not contain a definition for {}", (*item).fg(Color::Yellow)), Color::Red),
                    (item.span(), format!("A declaration of this item, {}, is missing from the class member", (*item).fg(Color::Yellow)), Color::Yellow),
                ],
                vec![format!("Consider adding the item like {}", format!("=> {} = ...", *item).fg(Color::Blue))],
            ),
            Error::RecursiveAlias(alias, ty, span) => (
                format!("Recursive type alias"),
                vec![
                    (ctx.datas.get_alias_span(alias), format!("Alias mentions itself, leading to an infinite expansion"), Color::Red),
                    (span, format!("Recursion occurs here"), Color::Yellow),
                ],
                {
                    let alias = ctx.datas.get_alias(alias).unwrap();
                    vec![format!(
                        "Type aliases expand eagerly. Consider using a data type like {} instead",
                        format!("data {} = {}", alias.name, display(alias.ty).substitute(ty, |f| write!(f, "{}", alias.name))).fg(Color::Blue),
                    )]
                },
            ),
            Error::DuplicateTypeName(name, old, new) => (
                format!("Type {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous declaration"), Color::Yellow),
                    (new, format!("Conflicting declaration"), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateDefName(name, old, new) => (
                format!("Definition {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous declaration"), Color::Yellow),
                    (new, format!("Conflicting declaration"), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateConsName(name, old, new) => (
                format!("Constructor {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous declaration"), Color::Yellow),
                    (new, format!("Conflicting declaration"), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateGenName(name, old, new) => (
                format!("Type parameter {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous type parameter"), Color::Yellow),
                    (new, format!("Conflicting type parameter"), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateClassName(name, old, new) => (
                format!("Type class {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous type class"), Color::Yellow),
                    (new, format!("Conflicting type class"), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateClassItem(name, old, new) => (
                format!("Item {} declared multiple times in class", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous item"), Color::Yellow),
                    (new, format!("Conflicting item"), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateMemberItem(name, old, new) => (
                format!("Item {} declared multiple times in class member", name.fg(Color::Red)),
                vec![
                    (old, format!("Previous item"), Color::Yellow),
                    (new, format!("Conflicting item"), Color::Red),
                ],
                vec![],
            ),
            Error::PatternNotSupported(lhs, op, rhs, span) => (
                format!("Arithmetic pattern {} {} {} is not supported", display(lhs).fg(Color::Red), (*op).fg(Color::Red), display(rhs).fg(Color::Red)),
                vec![(span, format!("Pattern {} used here", (*op).fg(Color::Red)), Color::Red)],
                vec![format!(
                    "Only specific arithmetic patterns, such as {}, are supported",
                    format!("Nat + Nat").fg(Color::Blue),
                )],
            ),
            Error::NotExhaustive(span, example, is_match) => (
                format!("{} is not exhaustive", if is_match { "Pattern match"} else { "Let" }),
                vec![(span, format!("Pattern {} not covered", example.display(ctx, is_match).fg(Color::Red)), Color::Red)],
                if is_match {
                    vec![format!(
                        "Add another arm like {} to ensure that this case is covered",
                        format!("| {} => ...", example.display(ctx, is_match)).fg(Color::Blue),
                    )]
                } else {
                    vec![format!("Let patterns must be exhaustive")]
                },
            ),
            Error::WrongNumberOfGenerics(a, a_count, b, b_count) => (
                format!("Wrong number of type parameters"),
                vec![
                    (a, format!("Provided with {} parameters", a_count), Color::Red),
                    (b, format!("Has {} parameter(s)", b_count), Color::Yellow),
                ],
                vec![],
            ),
            Error::DefTypeNotSpecified(def, usage, name) => (
                format!("Type of {} must be fully specified", name.fg(Color::Red)),
                vec![
                    (def, format!("Definition does not have a fully specified type hint"), Color::Red),
                    (usage, format!("Type must be fully known here"), Color::Yellow),
                ],
                vec![format!(
                    "Add a type hint to the def like {}",
                    format!("def {} : ...", name).fg(Color::Blue),
                )],
            ),
            Error::SelfNotValidHere(span) => (
                format!("Special type {} cannot be used here", "Self".fg(Color::Red)),
                vec![
                    (span, format!("Not valid in this context"), Color::Red),
                ],
                vec![format!("The {} type can only be used in type classes", "Self".fg(Color::Blue))],
            ),
            Error::NoEntryPoint(root_span) => (
                format!("No main definition"),
                vec![(root_span, format!("Does not contain a definition marked as the main entry point"), Color::Red)],
                vec![format!("Mark a definition as the main entry point with {}", "$[main]".fg(Color::Blue))],
            ),
            Error::MultipleEntryPoints(a, b) => (
                format!("Multiple entry points"),
                vec![
                    (a, format!("First entry point is here"), Color::Red),
                    (b, format!("Second entry point is here"), Color::Red),
                ],
                vec![format!("A program may only have a single entry point")],
            ),
            Error::GenericEntryPoint(name, gen) => (
                format!("Entry point {} cannot be generic", (*name).fg(Color::Red)),
                vec![
                    (gen, format!("Generics are not allowed here"), Color::Red),
                ],
                vec![format!("A program cannot be generic over types")],
            ),
            Error::AmbiguousClassItem(item, candidate_classes) => (
                format!("Class item {} is ambiguous", (*item).fg(Color::Red)),
                vec![
                    (item.span(), format!("Item could be from multiple classes"), Color::Red),
                ],
                vec![format!("Possible candidates are members of {}", candidate_classes
                    .into_iter()
                    .map(|class| format!("{}", (ctx.classes.get(class).name).fg(Color::Blue)))
                    .collect::<Vec<_>>()
                    .join(", "))],
            ),
            Error::InvalidIntrinsic(intrinsic) => (
                format!("Intrinsic {} is not valid", (*intrinsic).fg(Color::Red)),
                vec![
                    (intrinsic.span(), format!("No such intrinsic"), Color::Red),
                ],
                vec![format!("Maybe the wrong number of arguments were used?")],
            ),
            Error::Unsupported(span, feature) => (
                format!("Feature {} is not yet supported", feature.fg(Color::Yellow)),
                vec![
                    (span, format!("This is unsupported"), Color::Red),
                ],
                vec![],
            ),
            Error::NonNumeric(ty, span, num_litr) => (
                format!("Numeric literal of type {} cannot coerce to type {}", num_litr.fg(Color::Red), display(ty).fg(Color::Red)),
                vec![
                    (span, format!("Is not a valid {}", display(ty).fg(Color::Red)), Color::Red),
                    (ctx.tys.get_span(ty), format!("Coercion to {} is required here", display(ty).fg(Color::Yellow)), Color::Yellow),
                ],
                vec![format!("Numeric literals must be valid in the context of their use")],
            ),
            Error::MissingLangItem(name) => (
                format!("Lang item {} is missing", name.fg(Color::Yellow)),
                Vec::new(),
                vec![format!("All lang items must be defined")],
            ),
        };

        let mut report = Report::build(
            ReportKind::Error,
            spans.first().map(|s| s.0.src()).unwrap_or(main_src),
            spans.first().map(|s| s.0.start()).unwrap_or(0),
        )
            .with_code(3)
            .with_message(msg);

        for (span, msg, col) in spans {
            report = report.with_label(Label::new(span)
                .with_message(msg)
                .with_color(col));
        }

        for note in notes {
            report = report.with_note(note);
        }

        report
            .with_config(Config::default()
                .with_compact(false))
            .finish()
            .write(cache, writer)
            .unwrap();
    }
}
