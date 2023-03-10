use super::*;
use std::io::Write;

#[derive(Debug)]
pub enum Error {
    // Outer type, inner type
    CannotCoerce(TyId, TyId, Option<(TyId, TyId)>, EqInfo),
    CannotInfer(TyId, Option<Span>),
    CannotInferEffect(Span, Result<EffectInst, ()>),
    NotEffectful(TyId, Span, Span),
    Recursive(TyId, Span, Span),
    AliasNotPermitted(AliasId, Span),
    NoSuchItem(TyId, Span, SrcNode<Ident>),
    NoSuchField(TyId, Span, SrcNode<Ident>),
    NoSuchLocal(SrcNode<Ident>),
    WrongNumberOfParams(Span, usize, Span, usize),
    NoBranches(Span),
    // (obligation, type, obligation_origin, generic_definition
    TypeDoesNotFulfil(
        Option<(ClassId, Vec<TyId>, Vec<Option<EffectId>>)>,
        TyId,
        Span,
        Option<Span>,
        Span,
    ),
    CycleWhenResolving(TyId, (ClassId, Vec<TyId>, Vec<Option<EffectId>>), Span),
    NoSuchData(SrcNode<Ident>),
    NoSuchCons(SrcNode<Ident>),
    NoSuchClass(SrcNode<Ident>),
    NoSuchClassItem(SrcNode<Ident>, SrcNode<Ident>),
    NoSuchEffect(SrcNode<Ident>),
    AmbiguousClassItem(SrcNode<Ident>, Vec<ClassId>),
    MissingClassItem(Span, SrcNode<Ident>, SrcNode<Ident>),
    RecursiveAlias(AliasId, TyId, Span),
    DuplicateTypeName(Ident, Span, Span),
    DuplicateDefName(Ident, Span, Span),
    DuplicateConsName(Ident, Span, Span),
    DuplicateGenName(Ident, Span, Span),
    DuplicateClassName(Ident, Span, Span),
    DuplicateEffectDecl(Ident, Span, Span),
    DuplicateClassItem(Ident, Span, Span),
    DuplicateMemberItem(Ident, Span, Span),
    PatternNotSupported(TyId, SrcNode<ast::BinaryOp>, TyId, Span),
    // Span, uncovered example, hidden_outer
    NotExhaustive(Span, ExamplePat, bool),
    WrongNumberOfGenerics(Span, usize, Span, usize),
    DefTypeNotSpecified(SrcNode<DefId>, Span, Ident),
    SelfNotValidHere(Span),
    AssocNotValidHere(Span),
    NoEntryPoint(Span),
    MultipleEntryPoints(Span, Span),
    GenericEntryPoint(SrcNode<Ident>, Span),
    InvalidIntrinsic(SrcNode<Ident>),
    Unsupported(Span, &'static str),
    MissingLangItem(&'static str),
    NoBasin(Span),
    NotMentioned(SrcNode<Ident>),
}

impl Error {
    pub fn write<C: ariadne::Cache<SrcId>>(
        self,
        ctx: &Context,
        cache: C,
        main_src: SrcId,
        writer: impl Write,
    ) {
        use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Span};

        let display = |id| ctx.tys.display(ctx, id);

        let display_eff = |id| {
            if let Some(eff) = id {
                ctx.tys.display_eff(ctx, eff).to_string()
            } else {
                "!".to_string()
            }
        };

        let display_class = |class_id, gen_tys: &[_], gen_effs: &[_]| {
            format!(
                "{}{}",
                *ctx.classes.get(class_id).name,
                gen_tys
                    .iter()
                    .map(|ty| format!(" {}", display(*ty)))
                    .chain(gen_effs.iter().map(|eff| format!(" {}", display_eff(*eff))))
                    .collect::<String>(),
            )
        };

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
                            labels.push((at, "Coercion is required here".to_string(), Color::Cyan));
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
                        (ctx.tys.get_span(a), "Use of generic item".to_string(), Color::Red),
                        (origin, "Instantiation of this generic type".to_string(), Color::Yellow)
                    ],
                    None => vec![(ctx.tys.get_span(a), format!("{}", display(a)), Color::Red)],
                },
                vec![],
            ),
            Error::CannotInferEffect(span, _a) => (
                "Cannot infer effect".to_string(),
                vec![(span, "Cannot be inferred".to_string(), Color::Red)],
                vec![],
            ),
            Error::NotEffectful(ty, obj_span, span) => (
                format!("Type {} has no effects to be propagated", display(ty).fg(Color::Yellow)),
                vec![
                    (span, "Propagation is attempted here".to_string(), Color::Red),
                    (obj_span, format!("This is of type {} and so has no effects to propagate", display(ty).fg(Color::Yellow)), Color::Yellow),
                ],
                vec![format!("Only values with types like {} can have their effects propagated to the enclosing scope", "e ~ T".fg(Color::Cyan))],
            ),
            Error::Recursive(a, span, part) => (
                format!("Self-referencing type {} expands to have infinite size", display(a).fg(Color::Red)),
                vec![
                    (span, "Mentions itself".to_string(), Color::Red),
                    (part, "Self-reference occurs here".to_string(), Color::Yellow),
                ],
                vec![
                    "Types expand eagerly and so self-reference results in infinite size".to_string(),
                    format!("If this was intentional, consider using {} instead", "data".fg(Color::Cyan)),
                ],
            ),
            Error::AliasNotPermitted(alias, span) => {
                let alias = ctx.datas.get_alias(alias).unwrap();
                (
                    format!("Type alias {} not valid here", alias.name.fg(Color::Red)),
                    vec![
                        (span, "Not valid here".to_string(), Color::Red),
                    ],
                    vec![
                        "Types aliases are not valid as class members".to_string(),
                        format!("Consider using the full type, {}, instead", display(alias.ty).fg(Color::Blue)),
                    ],
                )
            },
            Error::NoSuchItem(a, record_span, field) => (
                format!("Type {} has no item named {}", display(a).fg(Color::Red), (*field).fg(Color::Red)),
                vec![
                    (record_span, format!("Has type {}", display(a).fg(Color::Yellow)), Color::Yellow),
                    (field.span(), "Item does not exist".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::NoSuchField(a, record_span, field) => (
                format!("Type {} has no field named {}", display(a).fg(Color::Red), (*field).fg(Color::Red)),
                vec![
                    (record_span, format!("Has type {}", display(a).fg(Color::Yellow)), Color::Yellow),
                    (field.span(), "Field does not exist".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::NoSuchLocal(local) => (
                format!("No such local {}", (*local).fg(Color::Red)),
                vec![(local.span(), "Scope does not contain this".to_string(), Color::Red)],
                vec![],
            ),
            Error::WrongNumberOfParams(a, a_count, b, b_count) => (
                "Pattern arms must all have the same number of parameters".to_string(),
                vec![
                    (a, format!("Has {} parameter(s)", a_count), Color::Red),
                    (b, format!("Has {} parameter(s)", b_count), Color::Red),
                ],
                vec![],
            ),
            Error::NoBranches(span) => (
                "Pattern match must have at least one branch".to_string(),
                vec![(span, "Must have a branch".to_string(), Color::Red)],
                vec![],
            ),
            Error::TypeDoesNotFulfil(class, ty, obl_span, gen_span, use_span) => {
                let class = if let Some((class_id, gen_tys, gen_effs)) = class {
                    display_class(class_id, &gen_tys, &gen_effs)
                } else {
                    "?".to_string()
                };
                (
                    format!("Type {} is not a member of {}", display(ty).fg(Color::Red), (&class).fg(Color::Cyan)),
                    {
                        let mut labels = vec![
                            (use_span, "Because it is used here".to_string(), Color::Yellow),
                            (ctx.tys.get_span(ty), format!(
                                "This is of type {}",
                                display(ty).fg(Color::Red),
                            ), Color::Red),
                            (obl_span, format!("{} is required to be a member of {} here", display(ty).fg(Color::Red), (&class).fg(Color::Cyan)), Color::Cyan),
                        ];
                        if let Some(gen_span) = gen_span {
                            labels.push((gen_span, format!(
                                "Consider adding a class constraint like {}",
                                format!("{} < {}", display(ty), class).fg(Color::Blue),
                            ), Color::Blue));
                        }
                        labels
                    },
                    vec!["Types must fulfil their class obligations".to_string()],
                )
            },
            Error::CycleWhenResolving(ty, (class_id, gen_tys, gen_effs), cycle_span) => (
                format!(
                    "Proving that type {} is a member of {} leads to cyclical reasoning",
                    display(ty).fg(Color::Red),
                    display_class(class_id, &gen_tys, &gen_effs).fg(Color::Cyan),
                ),
                vec![
                    (cycle_span, "This bound causes cyclical reasoning".to_string(), Color::Red),
                    (ctx.tys.get_span(ty), format!(
                        "This is of type {}",
                        display(ty).fg(Color::Red),
                    ), Color::Red),
                ],
                vec!["Types must fulfil their class obligations".to_string()],
            ),
            Error::NoSuchData(a) => (
                format!("No such type {}", (*a).fg(Color::Red)),
                vec![(a.span(), "Does not exist".to_string(), Color::Red)],
                vec![],
            ),
            Error::NoSuchCons(a) => (
                format!("No such constructor {}", (*a).fg(Color::Red)),
                vec![(a.span(), "Does not exist".to_string(), Color::Red)],
                vec![],
            ),
            Error::NoSuchClass(a) => (
                format!("No such class {}", (*a).fg(Color::Red)),
                vec![(a.span(), "Does not exist".to_string(), Color::Red)],
                vec![],
            ),
            Error::NoSuchClassItem(item, class) => (
                format!("No such item {} on class {}", (*item).fg(Color::Red), (*class).fg(Color::Red)),
                vec![
                    (item.span(), "This item is not required in the class contract".to_string(), Color::Red),
                    (class.span(), format!("Does not have an item named {}", (*item).fg(Color::Red)), Color::Yellow),
                ],
                vec!["Class members must provide only the items required by their class".to_string()],
            ),
            Error::MissingClassItem(member, class, item) => (
                format!("Member of class {} is missing class item {}", (*class).fg(Color::Red), (*item).fg(Color::Red)),
                vec![
                    (member, format!("This member does not contain a definition for {}", (*item).fg(Color::Yellow)), Color::Red),
                    (item.span(), format!("A declaration of this item, {}, is missing from the class member", (*item).fg(Color::Yellow)), Color::Yellow),
                ],
                vec![format!("Consider adding the item like {}", format!("=> {} = ...", *item).fg(Color::Blue))],
            ),
            Error::NoSuchEffect(a) => (
                format!("No such effect {}", (*a).fg(Color::Red)),
                vec![(a.span(), "Does not exist".to_string(), Color::Red)],
                vec![],
            ),
            Error::RecursiveAlias(alias, ty, span) => (
                "Recursive type alias".to_string(),
                vec![
                    (ctx.datas.get_alias_span(alias), "Alias mentions itself, leading to an infinite expansion".to_string(), Color::Red),
                    (span, "Recursion occurs here".to_string(), Color::Yellow),
                ],
                {
                    let alias = ctx.datas.get_alias(alias).unwrap();
                    vec![format!(
                        "Type aliases expand eagerly. Consider using a data type like {} instead",
                        format!("data {} = {}", *alias.name, display(alias.ty).substitute(ty, |f| write!(f, "{}", *alias.name))).fg(Color::Blue),
                    )]
                },
            ),
            Error::DuplicateTypeName(name, old, new) => (
                format!("Type {} cannot be declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, "Previous declaration".to_string(), Color::Yellow),
                    (new, "Conflicting declaration".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateDefName(name, old, new) => (
                format!("Definition {} cannot be declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, "Previous declaration".to_string(), Color::Yellow),
                    (new, "Conflicting declaration".to_string(), Color::Red),
                    (new, format!("Consider renaming this, perhaps to {}?", format!("{}2", name).fg(Color::Cyan)), Color::Cyan),
                ],
                vec![],
            ),
            Error::DuplicateConsName(name, old, new) => (
                format!("Constructor {} cannot be declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, "Previous declaration".to_string(), Color::Yellow),
                    (new, "Conflicting declaration".to_string(), Color::Red),
                    (new, format!("Consider renaming this, perhaps to {}?", format!("{}2", name).fg(Color::Cyan)), Color::Cyan),
                ],
                vec![],
            ),
            Error::DuplicateGenName(name, old, new) => (
                format!("Type parameter {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, "Previous type parameter".to_string(), Color::Yellow),
                    (new, "Conflicting type parameter".to_string(), Color::Red),
                    (new, format!("Consider renaming this, perhaps to {}?", format!("{}2", name).fg(Color::Cyan)), Color::Cyan),
                ],
                vec![],
            ),
            Error::DuplicateClassName(name, old, new) => (
                format!("Type class {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, "Previous type class".to_string(), Color::Yellow),
                    (new, "Conflicting type class".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateEffectDecl(name, old, new) => (
                format!("Effect {} declared multiple times", name.fg(Color::Red)),
                vec![
                    (old, "Previous effect".to_string(), Color::Yellow),
                    (new, "Conflicting effect".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateClassItem(name, old, new) => (
                format!("Item {} declared multiple times in class", name.fg(Color::Red)),
                vec![
                    (old, "Previous item".to_string(), Color::Yellow),
                    (new, "Conflicting item".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::DuplicateMemberItem(name, old, new) => (
                format!("Item {} declared multiple times in class member", name.fg(Color::Red)),
                vec![
                    (old, "Previous item".to_string(), Color::Yellow),
                    (new, "Conflicting item".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::PatternNotSupported(lhs, op, rhs, span) => (
                format!("Arithmetic pattern {} {} {} is not supported", display(lhs).fg(Color::Red), (*op).fg(Color::Red), display(rhs).fg(Color::Red)),
                vec![(span, format!("Pattern {} used here", (*op).fg(Color::Red)), Color::Red)],
                vec![format!(
                    "Only specific arithmetic patterns, such as {}, are supported",
                    "Nat + Nat".to_string().fg(Color::Blue),
                )],
            ),
            Error::NotExhaustive(span, example, hidden_outer) => (
                format!("{} is not exhaustive", if hidden_outer { "Pattern match"} else { "Let" }),
                vec![(span, format!("Pattern {} not covered", example.display(ctx, hidden_outer).fg(Color::Red)), Color::Red)],
                if hidden_outer {
                    vec![format!(
                        "Add another arm like {} to ensure that this case is covered",
                        format!("| {} => ...", example.display(ctx, hidden_outer)).fg(Color::Blue),
                    )]
                } else {
                    vec!["Let patterns must be exhaustive".to_string()]
                },
            ),
            Error::WrongNumberOfGenerics(a, a_count, b, b_count) => (
                "Wrong number of type parameters".to_string(),
                vec![
                    (a, format!("Provided with {} parameters", a_count), Color::Red),
                    (b, format!("Has {} parameter(s)", b_count), Color::Yellow),
                ],
                vec![],
            ),
            Error::DefTypeNotSpecified(def, usage, name) => {
                let ty_str = ctx.defs
                    .get(*def)
                    .body
                    .as_ref()
                    .map(|body| display(body.meta().1).to_string())
                    .unwrap_or_else(|| "...".to_string());
                (
                    format!("Type of {} must be fully specified", name.fg(Color::Red)),
                    vec![
                        (def.span(), "Definition does not have a fully specified type hint".to_string(), Color::Red),
                        (usage, "Type must be fully known here".to_string(), Color::Yellow),
                    ],
                    vec![format!(
                        "Add a type hint to the def like {}",
                        format!("def {} : {}", name, ty_str).fg(Color::Blue),
                    )],
                )
            },
            Error::SelfNotValidHere(span) => (
                format!("Special type {} cannot be used here", "Self".fg(Color::Red)),
                vec![
                    (span, "Not valid in this context".to_string(), Color::Red),
                ],
                vec![format!("The {} type can only be used in type classes", "Self".fg(Color::Blue))],
            ),
            Error::AssocNotValidHere(span) => (
                "Associated types cannot be used here".to_string(),
                vec![
                    (span, "Not valid in this context".to_string(), Color::Red),
                ],
                vec!["Associated types may not be used as class members directly".to_string()],
            ),
            Error::NoEntryPoint(root_span) => (
                "No main definition".to_string(),
                vec![(root_span, "Does not contain a definition marked as the main entry point".to_string(), Color::Red)],
                vec![format!("Mark a definition as the main entry point with {}", "$[main]".fg(Color::Blue))],
            ),
            Error::MultipleEntryPoints(a, b) => (
                "Multiple entry points".to_string(),
                vec![
                    (a, "First entry point is here".to_string(), Color::Red),
                    (b, "Second entry point is here".to_string(), Color::Red),
                ],
                vec!["A program may only have a single entry point".to_string()],
            ),
            Error::GenericEntryPoint(name, gen) => (
                format!("Entry point {} cannot be generic", (*name).fg(Color::Red)),
                vec![
                    (gen, "Generics are not allowed here".to_string(), Color::Red),
                ],
                vec!["A program cannot be generic over types".to_string()],
            ),
            Error::AmbiguousClassItem(item, candidate_classes) => (
                format!("Class item {} is ambiguous", (*item).fg(Color::Red)),
                vec![
                    (item.span(), "Item could be from multiple classes".to_string(), Color::Red),
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
                    (intrinsic.span(), "No such intrinsic".to_string(), Color::Red),
                ],
                vec!["Maybe the wrong number of arguments were used?".to_string()],
            ),
            Error::Unsupported(span, feature) => (
                format!("Feature {} is not yet supported", feature.fg(Color::Yellow)),
                vec![
                    (span, "This is unsupported".to_string(), Color::Red),
                ],
                vec![],
            ),
            Error::MissingLangItem(name) => (
                format!("Lang item {} is missing", name.fg(Color::Yellow)),
                Vec::new(),
                vec!["All lang items must be defined".to_string()],
            ),
            Error::NoBasin(span) => (
                "Effect propagated, but nothing catches it".to_string(),
                vec![
                    (span, "Nothing catches this propagation".to_string(), Color::Red),
                ],
                vec![format!("Place this expression within a {} block", "effect { ... }".fg(Color::Blue))],
            ),
            Error::NotMentioned(param) => (
                format!("Generic parameter {} not mentioned in item", (*param).fg(Color::Red)),
                vec![
                    (param.span(), "The item this generic parameterises does not mention it".to_string(), Color::Red),
                ],
                vec!["Generic parameters must always be mentioned by the thing they parameterise".to_string()],
            ),
        };

        let mut report = Report::build(
            ReportKind::Error,
            spans.first().map(|s| s.0.src()).unwrap_or(main_src),
            spans.first().map(|s| s.0.start()).unwrap_or(0),
        )
        .with_code(3)
        .with_message(msg);

        for (i, (span, msg, col)) in spans.into_iter().enumerate() {
            report = report.with_label(
                Label::new(span)
                    .with_message(msg)
                    .with_order(i as i32)
                    .with_color(col),
            );
        }

        for note in notes {
            report = report.with_note(note);
        }

        report
            .with_config(Config::default().with_compact(false))
            .finish()
            .write(cache, writer)
            .unwrap();
    }
}
