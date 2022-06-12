use super::*;

pub trait Parser<T> = chumsky::Parser<Token, T, Error = Error> + Clone;

pub fn literal_parser() -> impl Parser<ast::Literal> {
    select! {
        Token::Nat(x) => ast::Literal::Nat(x),
        Token::Int(x) => ast::Literal::Int(x),
        Token::Real(x) => ast::Literal::Real(x.parse().expect("Real could not be parsed as f64")),
        Token::Char(x) => ast::Literal::Char(x),
        Token::Str(x) => ast::Literal::Str(x),
    }
        .map_err(|e: Error| e.expected(Pattern::Literal))
}

pub fn term_ident_parser() -> impl Parser<ast::Ident> {
    select! { Token::TermIdent(x) => x }
        .map_err(|e: Error| e.expected(Pattern::TermIdent))
}

pub fn type_ident_parser() -> impl Parser<ast::Ident> {
    select! { Token::TypeIdent(x) => x }
        .map_err(|e: Error| e.expected(Pattern::TypeIdent))
}

pub fn nested_parser<'a, T: 'a>(parser: impl Parser<T> + 'a, delimiter: Delimiter, f: impl Fn(Span) -> T + Clone + 'a) -> impl Parser<T> + 'a {
    parser
        .delimited_by(just(Token::Open(delimiter)), just(Token::Close(delimiter)))
        .recover_with(nested_delimiters(
            Token::Open(delimiter), Token::Close(delimiter),
            [
                (Token::Open(Delimiter::Paren), Token::Close(Delimiter::Paren)),
                (Token::Open(Delimiter::Brack), Token::Close(Delimiter::Brack)),
                (Token::Open(Delimiter::Brace), Token::Close(Delimiter::Brace)),
            ],
            f,
        ))
        .boxed()
}

pub fn type_parser() -> impl Parser<ast::Type> {
    recursive(|ty| {
        let data = type_ident_parser() // TODO: Replace with `data_item_parser` when ready
            .map_with_span(SrcNode::new)
            .map(|data_name| ast::Type::Data(data_name, Vec::new()));

        let list = nested_parser(
            ty.clone()
                .map_with_span(SrcNode::new)
                .map(Some),
            Delimiter::Brack,
            |_| None,
        )
            .map(|ty| ty.map(ast::Type::List).unwrap_or(ast::Type::Error));

        let tuple = nested_parser(
            ty.clone()
                .map_with_span(SrcNode::new)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .then_ignore(just(Token::Comma).or_not())
                .map(Some),
            Delimiter::Paren,
            |_| None,
        )
            .map(|tys| tys.map(ast::Type::Tuple).unwrap_or(ast::Type::Error));

        let record = nested_parser(
            term_ident_parser()
                .map_with_span(SrcNode::new)
                .then_ignore(just(Token::Colon))
                .then(ty.clone().map_with_span(SrcNode::new))
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .then_ignore(just(Token::Comma).or_not())
                .map(Some)
                .boxed(),
            Delimiter::Brace,
            |_| None,
        )
            .map(|tys| tys.map(ast::Type::Record).unwrap_or(ast::Type::Error));

        let unknown = just(Token::Question)
            .map(|_| ast::Type::Unknown);

        let universe = just(Token::At)
            .map(|_| ast::Type::Universe);

        let paren_ty = nested_parser(
            ty.clone().map(Some),
            Delimiter::Paren,
            |_| None,
        )
            .map(|ty| ty.unwrap_or(ast::Type::Error));

        let atom = paren_ty
            .or(data)
            .or(list)
            .or(tuple)
            .or(record)
            .or(unknown)
            .or(universe)
            .or(select! { Token::Error(_) => () }.map(|_| ast::Type::Error))
            .map_with_span(SrcNode::new)
            .boxed();

        let assoc = atom
            .then(just(Token::Op(Op::Dot))
                .ignore_then(type_ident_parser().map_with_span(SrcNode::new)
                    .then(ty.clone().map_with_span(SrcNode::new).repeated())
                    .delimited_by(just(Token::Op(Op::Less)), just(Token::Op(Op::More))))
                .map_with_span(SrcNode::new)
                .or_not()
                .then(just(Token::Op(Op::Dot)).ignore_then(type_ident_parser().map_with_span(SrcNode::new)))
                .repeated())
            .foldl(|inner, (class, assoc)| {
                let class = class.map(|class| class.map(|(name, params)| ast::ClassInst {
                    name,
                    params,
                }));
                let span = inner.span().union(assoc.span());
                SrcNode::new(ast::Type::Assoc(inner, class, assoc), span)
            });

        let data = type_ident_parser() // TODO: Replace with `data_item_parser` when ready
            .map_with_span(SrcNode::new)
            .then(assoc.clone().repeated().at_least(1))
            .map(|(data, params)| ast::Type::Data(data, params))
            .map_with_span(SrcNode::new)
            .or(assoc)
            .boxed();

        let effect = term_ident_parser() // TODO: Replace with `term_item_parser` when ready
            .map_with_span(SrcNode::new)
            .then(data.clone().repeated())
            .then_ignore(just(Token::Tilde))
            .then(data.clone())
            .map(|((eff, params), out)| ast::Type::Effect(eff, params, out))
            .map_with_span(SrcNode::new)
            .or(data)
            .boxed();

        effect.clone()
            .then(just(Token::Op(Op::RArrow))
                .ignore_then(ty.clone().map_with_span(SrcNode::new))
                .repeated())
            .foldl(|i, o| {
                let span = i.span().union(o.span());
                SrcNode::new(ast::Type::Func(i, o), span)
            })
            .or(effect)
            .map(|ty| ty.into_inner())
    })
        .labelled("type")
}

pub fn class_inst_parser() -> impl Parser<ast::ClassInst> {
    type_ident_parser()
        .map_with_span(SrcNode::new)
        .then(type_parser()
            .map_with_span(SrcNode::new)
            .repeated())
        .map(|(name, params)| ast::ClassInst {
            name,
            params,
        })
}

pub fn ty_hint_parser() -> impl Parser<Option<SrcNode<ast::Type>>> {
    just(Token::Colon)
        .ignore_then(type_parser()
            .map_with_span(SrcNode::new))
        .or_not()
}

pub fn always_branches<T>(branch: impl Parser<T> + Clone) -> impl Parser<Vec<T>> {
    just(Token::Pipe)
        .ignore_then(branch.clone())
        .repeated()
        .then(just(Token::EndPipe)
            .ignore_then(branch.clone())
            .or_not())
        .try_map(|(mut init, end), span| if let Some(end) = end {
            init.push(end);
            Ok(init)
        } else {
            Err(Error::new(ErrorKind::NoEndBranch, span))
        })
}

pub fn branches<T>(branch: impl Parser<T> + Clone) -> impl Parser<Vec<T>> {
    branch.clone().map(|x| vec![x])
        .or(always_branches(branch))
}

pub fn binding_parser() -> impl Parser<ast::Binding> {
    let binding = recursive(move |binding| {
        let wildcard = just(Token::Wildcard)
            .map_with_span(|_, span| SrcNode::new(ast::Pat::Wildcard, span));

        let litr = literal_parser()
            .map_with_span(|litr, span| SrcNode::new(ast::Pat::Literal(litr), span));

        let paren_binding = nested_parser(
            binding.clone()
                .then(ty_hint_parser())
                .map(|(binding, ty): (SrcNode<ast::Binding>, _)| ast::Binding {
                    ty,
                    ..binding.into_inner()
                })
                .map_with_span(SrcNode::new)
                .map(Some),
            Delimiter::Paren,
            |_| None,
        )
            .map(|x| x.map(ast::Pat::Single).unwrap_or(ast::Pat::Error))
            .map_with_span(SrcNode::new);

        let tuple = nested_parser(
            binding.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .map(Some),
            Delimiter::Paren,
            |_| None,
        )
            .map(|x| x.map(ast::Pat::Tuple).unwrap_or(ast::Pat::Error))
            .map_with_span(SrcNode::new);

        let record = nested_parser(
            term_ident_parser()
                .map_with_span(SrcNode::new)
                .then(ty_hint_parser())
                .then(just(Token::Tilde)
                    .ignore_then(binding.clone())
                    .or_not())
                .map_with_span(|((field, ty), binding), span| {
                    let binding = binding.unwrap_or_else(|| SrcNode::new(ast::Binding {
                        pat: SrcNode::new(ast::Pat::Wildcard, field.span()),
                        name: Some(field.clone()),
                        ty,
                    }, span));

                    (field, binding)
                })
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .then_ignore(just(Token::Comma).or_not())
                .map(Some)
                .boxed(),
            Delimiter::Brace,
            |_| None,
        )
            .map(|x| x.map(ast::Pat::Record).unwrap_or(ast::Pat::Error))
            .map_with_span(SrcNode::new);

        let list = nested_parser(
            binding.clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .then(just(Token::Op(Op::Ellipsis))
                    .ignore_then(binding.clone().or_not())
                    .or_not())
                .map(Some)
                .boxed(),
            Delimiter::Brack,
            |_| None,
        )
            .map(|x| x
                .map(|(items, tail)| match tail {
                    Some(tail) => ast::Pat::ListFront(items, tail),
                    None => ast::Pat::ListExact(items),
                })
                .unwrap_or(ast::Pat::Error))
            .map_with_span(SrcNode::new);

        let deconstruct = type_ident_parser() // TODO: Replace with `data_item_parser` when ready
            .map_with_span(SrcNode::new)
            .then(binding.or_not())
            .map_with_span(|(data, inner), span| {
                let inner = inner.unwrap_or_else(|| SrcNode::new(ast::Binding {
                    pat: SrcNode::new(ast::Pat::Tuple(Vec::new()), data.span()),
                    name: None,
                    ty: None,
                }, data.span()));

                SrcNode::new(ast::Pat::Deconstruct(data, inner), span)
            });

        let atom = wildcard
            .or(litr)
            .or(paren_binding)
            .or(tuple)
            .or(record)
            .or(list)
            .or(deconstruct)
            .or(select! { Token::Error(_) => () }.map_with_span(|_, span| SrcNode::new(ast::Pat::Error, span)))
            .map(|atom| (atom, None))
            .or(term_ident_parser().map_with_span(|ident, span| {
                (SrcNode::new(ast::Pat::Wildcard, span), Some(SrcNode::new(ident, span)))
            }))
            .boxed();

        let sum = atom
            .then(just(Token::Op(Op::Add))
                .to(ast::BinaryOp::Add)
                .map_with_span(SrcNode::new)
                .then(literal_parser().map_with_span(SrcNode::new))
                .repeated())
            .foldl(|(lhs_pat, lhs_name), (op, rhs)| {
                let span = lhs_pat.span().union(op.span()).union(rhs.span());
                let lhs_span = lhs_pat.span();
                (SrcNode::new(ast::Pat::Binary(
                    op,
                    SrcNode::new(ast::Binding {
                        pat: lhs_pat,
                        name: lhs_name,
                        ty: None,
                    }, lhs_span),
                    rhs,
                ), span), None)
            });

        let pat = sum;

        // Bound pattern
        let binding = term_ident_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Tilde))
            .then(pat.clone())
            .map(|(binding, (pat, name))| {
                let pat_span = pat.span();
                let inner_binding = SrcNode::new(ast::Binding {
                    pat,
                    name,
                    ty: None,
                }, pat_span);
                (SrcNode::new(ast::Pat::Single(inner_binding), pat_span), Some(binding))
            })
            // Unbound pattern
            .or(pat)
            // Ident
            .or(term_ident_parser().map_with_span(|name, span| (
                SrcNode::new(ast::Pat::Wildcard, span),
                if *name == "_" {
                    None
                } else {
                    Some(SrcNode::new(name, span))
                },
            )))
            // TODO: Resolve ambiguity
            // .then(ty_hint_parser())
            .map_with_span(|/*(*/(pat, name)/*, ty)*/, span| SrcNode::new(ast::Binding {
                pat,
                name,
                ty: None,
            }, span))
            .boxed();

        binding
    });

    // Type hint
    let binding = binding
        .map(|expr| expr.into_inner())
        .then(ty_hint_parser())
        .map(|(binding, ty)| ast::Binding {
            ty,
            ..binding
        });

    // Union pattern
    binding
        .labelled("pattern")
}

pub fn expr_parser() -> impl Parser<ast::Expr> {
    recursive(|expr| {
        let litr = literal_parser().map(ast::Expr::Literal);
        let ident = term_ident_parser().map(ast::Expr::Local);

        let paren_exp_list = nested_parser(
            expr
                .clone()
                .map_with_span(SrcNode::new)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .map(Some),
            Delimiter::Paren,
            |_| None,
        );

        let tuple = paren_exp_list
            .clone()
            .map(|x| x.map(ast::Expr::Tuple).unwrap_or(ast::Expr::Error))
            .labelled("tuple");

        let fields = nested_parser(
            term_ident_parser()
                .map_with_span(SrcNode::new)
                .then(just(Token::Colon)
                    .ignore_then(expr.clone().map_with_span(SrcNode::new))
                    .or_not())
                .map(|(field, val)| match val {
                    Some(val) => (field, val),
                    None => {
                        let val = SrcNode::new(ast::Expr::Local(*field), field.span());
                        (field, val)
                    },
                })
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .map(Some)
                .boxed(),
            Delimiter::Brace,
            |_| None,
        );

        let record = fields
            .clone()
            .map(|fields| fields.map(ast::Expr::Record).unwrap_or(ast::Expr::Error))
            .labelled("record");

        let list = nested_parser(
            expr
                .clone()
                .map_with_span(SrcNode::new)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .then(just(Token::Op(Op::Ellipsis))
                    .ignore_then(expr.clone()
                        .map_with_span(SrcNode::new)
                        .separated_by(just(Token::Comma))
                        .allow_trailing())
                    .or_not())
                .map(Some),
            Delimiter::Brack,
            |_| None,
        )
            .map(|x| match x {
                Some((items, tails)) => ast::Expr::List(items, tails.unwrap_or_else(Vec::new)),
                None => ast::Expr::Error,
            })
            .labelled("list");

        let branch = binding_parser()
            .map_with_span(SrcNode::new)
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Op(Op::RFlow)))
            .then(expr
                .clone()
                .map_with_span(SrcNode::new))
            .boxed();

        let branches = branches(branch)
            .or(just(Token::Pipe).map(|_| Vec::new()));

        let func = just(Token::Fn)
            .ignore_then(branches.clone().map_with_span(SrcNode::new))
            .map(ast::Expr::Func);

        let class_access = type_parser()
            .map_with_span(SrcNode::new)
            .delimited_by(just(Token::Op(Op::Less)), just(Token::Op(Op::More)))
            .or(type_ident_parser()
                .map_with_span(SrcNode::new)
                .map(|ty| {
                    let ty_span = ty.span();
                    SrcNode::new(ast::Type::Data(ty, Vec::new()), ty_span)
                }))
            .then(just(Token::Op(Op::Dot))
                .ignore_then(term_ident_parser().map_with_span(SrcNode::new)))
            .map(|(ty, field)| ast::Expr::ClassAccess(ty, field));

        let let_ = just(Token::Let)
            .ignore_then(binding_parser().map_with_span(SrcNode::new)
                .then_ignore(just(Token::Op(Op::Eq)))
                .then(expr.clone().map_with_span(SrcNode::new))
                .separated_by(just(Token::Comma))
                .allow_trailing())
            .then_ignore(just(Token::In))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|(bindings, then)| ast::Expr::Let(bindings, then))
            .boxed();

        let if_ = just(Token::If)
            .ignore_then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Then))
            .then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Else))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|((pred, a), b)| ast::Expr::If(pred, a, b))
            .boxed();

        let match_ = just(Token::Match)
            .ignore_then(expr.clone().map_with_span(SrcNode::new)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .map_with_span(SrcNode::new))
            .then_ignore(just(Token::In))
            .then(branches)
            .map(|(inputs, branches)| ast::Expr::Match(inputs, branches))
            .boxed();

        let intrinsic = select! { Token::Intrinsic(name) => name }
            .map_with_span(SrcNode::new)
            .then(paren_exp_list.clone().or_not())
            .map(|(name, args)| ast::Expr::Intrinsic(name, args.flatten().unwrap_or_default()));

        enum DoItem {
            Stmt(SrcNode<ast::Expr>),
            Bind(SrcNode<ast::Binding>, SrcNode<ast::Expr>),
        }

        let do_bind = just(Token::Let)
            .ignore_then(binding_parser().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Op(Op::LArrow)))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|(lhs, rhs)| DoItem::Bind(lhs, rhs));

        let do_stmt = expr.clone()
            .map_with_span(SrcNode::new)
            .map(DoItem::Stmt);

        let do_chain = do_bind
            .or(do_stmt)
            .then_ignore(just(Token::Semicolon))
            .repeated()
            .then(expr.clone()
                .map_with_span(SrcNode::new)
                .or_not());

        let do_ = just(Token::Do)
            .ignore_then(nested_parser(do_chain.map(Some), Delimiter::Brace, |_| None))
            .map_with_span(|x, span| if let Some((items, tail)) = x {
                let mut expr = match tail {
                    Some(tail) => tail,
                    None => SrcNode::new(ast::Expr::Apply(
                        SrcNode::new(ast::Expr::LangDef(ast::LangDef::IoUnit), span),
                        SrcNode::new(ast::Expr::Tuple(Vec::new()), span),
                    ), span),
                };

                for item in items.into_iter().rev() {
                    expr = match item {
                        DoItem::Stmt(rhs) => {
                            let expr_span = rhs.span().union(expr.span());
                            let binding = SrcNode::new(ast::Binding {
                                pat: SrcNode::new(ast::Pat::Wildcard, expr_span),
                                name: None,
                                ty: None,
                            }, expr_span);
                            SrcNode::new(ast::Expr::Apply(
                                SrcNode::new(ast::Expr::Apply(
                                    SrcNode::new(ast::Expr::LangDef(ast::LangDef::IoBind), expr.span()),
                                    rhs,
                                ), expr_span),
                                SrcNode::new(ast::Expr::Func(SrcNode::new(vec![(SrcNode::new(vec![binding], expr.span()), expr)], expr_span)), expr_span),
                            ), expr_span)
                        },
                        DoItem::Bind(binding, rhs) => {
                            let binding_span = binding.span().union(rhs.span());
                            let expr_span = binding.span().union(expr.span());
                            SrcNode::new(ast::Expr::Apply(
                                SrcNode::new(ast::Expr::Apply(
                                    SrcNode::new(ast::Expr::LangDef(ast::LangDef::IoBind), binding_span),
                                    rhs,
                                ), binding_span),
                                SrcNode::new(ast::Expr::Func(SrcNode::new(vec![(SrcNode::new(vec![binding], expr.span()), expr)], expr_span)), expr_span),
                            ), expr_span)
                        },
                    };
                }

                expr.into_inner()
            } else {
                ast::Expr::Error
            });

        let return_ = just(Token::Return)
            .ignore_then(expr.clone()
                .map_with_span(SrcNode::new))
            .map_with_span(|expr, span| ast::Expr::Apply(
                SrcNode::new(ast::Expr::LangDef(ast::LangDef::IoUnit), span),
                expr,
            ));

        let cons_unit = type_ident_parser()
            .map_with_span(SrcNode::new)
            .map(|cons| {
                let span = cons.span();
                ast::Expr::Cons(cons, SrcNode::new(ast::Expr::Tuple(Vec::new()), span))
            });

        // @{ x; y; z }
        // TODO: Come up with a better syntax
        let block = just(Token::At)
            .ignore_then(nested_parser(
                expr
                    .clone()
                    .then_ignore(just(Token::Semicolon))
                    .map_with_span(SrcNode::new)
                    .repeated()
                    .then(expr.clone()
                        .map_with_span(SrcNode::new)
                        .or_not())
                    .map_with_span(|(init, end), span| {
                        let last = if let Some(end) = end {
                            end
                        } else {
                            SrcNode::new(ast::Expr::Tuple(Vec::new()), span)
                        };
                        ast::Expr::Block(init, last)
                    })
                    .map(Some),
                Delimiter::Brace,
                |_| None,
            ))
            .map(|block| block.unwrap_or(ast::Expr::Error));

        let atom = litr
            .or(ident)
            .or(nested_parser(expr.clone(), Delimiter::Paren, |_| ast::Expr::Error))
            .or(tuple)
            .or(record)
            .or(list)
            .or(let_)
            .or(if_)
            .or(match_)
            .or(func)
            .or(class_access)
            .or(intrinsic)
            .or(do_)
            .or(return_)
            .or(cons_unit)
            .or(block)
            .or(select! { Token::Error(_) => () }.map(|_| ast::Expr::Error))
            .map_with_span(SrcNode::new)
            .boxed();

        // Apply direct (a pattern like `f(arg)` more eagerly binds than a simple application chain
        let direct = atom
            .then(paren_exp_list.clone().or_not())
            .map_with_span(|(expr, args), span| match args {
                Some(Some(args)) => {
                    let arg_count = args.len();
                    args
                        .into_iter()
                        .enumerate()
                        .fold(expr, |f, (i, arg)| {
                            let span = if i == arg_count - 1 {
                                span
                            } else {
                                f.span().union(arg.span())
                            };
                            SrcNode::new(ast::Expr::Apply(f, arg), span)
                        })
                },
                Some(None) => SrcNode::new(ast::Expr::Error, span),
                None => expr,
            });

        enum Chain {
            Field(SrcNode<ast::Ident>),
            Infix(SrcNode<ast::Expr>),
            Apply(Option<Vec<SrcNode<ast::Expr>>>, Span),
        }

        let field = term_ident_parser().or(select! { Token::Nat(x) => ast::Ident::new(format!("{}", x)) });
        let chain = just(Token::Op(Op::Dot))
            .ignore_then(field.map_with_span(SrcNode::new))
            .map(Chain::Field)
            .or(just(Token::Op(Op::RArrow)).ignore_then(direct.clone())
                .map(Chain::Infix))
            .or(paren_exp_list
                .map_with_span(|args, span| Chain::Apply(args, span)))
            .boxed();

        let chained = direct
            .then(chain.repeated())
            .foldl(|expr, chain| match chain {
                Chain::Field(field) => {
                    let span = expr.span().union(field.span());
                    SrcNode::new(ast::Expr::Access(expr, field), span)
                },
                Chain::Infix(f) => {
                    let span = expr.span().union(f.span());
                    SrcNode::new(ast::Expr::Apply(f, expr), span)
                },
                Chain::Apply(None, _span) => SrcNode::new(ast::Expr::Error, expr.span()),
                Chain::Apply(Some(args), outer_span) => {
                    let arg_count = args.len();
                    args
                        .into_iter()
                        .enumerate()
                        .fold(expr, |f, (i, arg)| {
                            let span = if i == arg_count - 1 {
                                outer_span
                            } else {
                                f.span().union(arg.span())
                            };
                            SrcNode::new(ast::Expr::Apply(f, arg), span)
                        })
                },
            })
            .boxed();

        // Propagated effects
        let op = just(Token::Op(Op::Not)).to(ast::UnaryOp::Propagate)
            .map_with_span(SrcNode::new);
        let propagated = chained.clone()
            .then(op.repeated())
            .foldl(|a, op| {
                let span = a.span().union(op.span());
                SrcNode::new(ast::Expr::Unary(op, a), span)
            })
            .boxed();

        // Unary
        let op = just(Token::Op(Op::Sub)).to(ast::UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(ast::UnaryOp::Not))
            .map_with_span(SrcNode::new);
        let unary = op.repeated()
            .then(propagated.labelled("unary operand"))
            .foldr(|op, expr| {
                let span = op.span().union(expr.span());
                SrcNode::new(ast::Expr::Unary(op, expr), span)
            })
            .boxed();

        // Product
        let op = just(Token::Op(Op::Mul)).to(ast::BinaryOp::Mul)
            .or(just(Token::Op(Op::Div)).to(ast::BinaryOp::Div))
            .or(just(Token::Op(Op::Rem)).to(ast::BinaryOp::Rem))
            .map_with_span(SrcNode::new);
        let product = unary.clone()
            .then(op.then(unary.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        // Sum
        let op = just(Token::Op(Op::Add)).to(ast::BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(ast::BinaryOp::Sub))
            .map_with_span(SrcNode::new);
        let sum = product.clone()
            .then(op.then(product.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        // List joining
        let op = just(Token::Op(Op::Join)).to(ast::BinaryOp::Join)
            .map_with_span(SrcNode::new);
        let join = sum.clone()
            .then(op.then(sum.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        // Comparison
        let op = just(Token::Op(Op::Less)).to(ast::BinaryOp::Less)
            .or(just(Token::Op(Op::LessEq)).to(ast::BinaryOp::LessEq))
            .or(just(Token::Op(Op::More)).to(ast::BinaryOp::More))
            .or(just(Token::Op(Op::MoreEq)).to(ast::BinaryOp::MoreEq))
            .or(just(Token::Op(Op::Eq)).to(ast::BinaryOp::Eq))
            .or(just(Token::Op(Op::NotEq)).to(ast::BinaryOp::NotEq))
            .map_with_span(SrcNode::new);
        let comparison = join.clone()
            .then(op.then(join.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        // Logical
        let op = just(Token::Op(Op::And)).to(ast::BinaryOp::And)
            .or(just(Token::Op(Op::Or)).to(ast::BinaryOp::Or))
            .or(just(Token::Op(Op::Xor)).to(ast::BinaryOp::Xor))
            .map_with_span(SrcNode::new);
        let logical = comparison.clone()
            .then(op.then(comparison.labelled("binary operand")).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            })
            .boxed();

        let with = logical
            .then(just(Token::With).ignore_then(fields).or_not())
            .map_with_span(|(expr, fields), span| if let Some(fields) = fields {
                SrcNode::new(if let Some(fields) = fields {
                    ast::Expr::Update(expr, fields)
                } else {
                    ast::Expr::Error
                }, span)
            } else {
                expr
            })
            .boxed();

        let cons = type_ident_parser()
            .map_with_span(SrcNode::new)
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|(cons, expr)| ast::Expr::Cons(cons, expr))
            .map_with_span(SrcNode::new)
            .or(with)
            .boxed();

        let handle = cons.then(just(Token::Handle)
                .ignore_then(term_ident_parser().map_with_span(SrcNode::new))
                .then(type_parser()
                    .map_with_span(SrcNode::new)
                    .repeated())
                .then_ignore(just(Token::With))
                .then(binding_parser().map_with_span(SrcNode::new))
                .then_ignore(just(Token::Op(Op::RFlow)))
                .then(expr.clone().map_with_span(SrcNode::new))
                .or_not())
            .map_with_span(|(expr, handle), span| if let Some((((eff_name, eff_args), send), recv)) = handle {
                SrcNode::new(ast::Expr::Handle {
                    expr,
                    eff_name,
                    eff_args,
                    send,
                    recv,
                }, span)
            } else {
                expr
            });

        handle
            .map(|b| b.into_inner())
    })
        .labelled("expression")
}

pub fn obligation_parser() -> impl Parser<Vec<SrcNode<ast::ClassInst>>> {
    just(Token::Op(Op::Less))
        .ignore_then(class_inst_parser()
            .map_with_span(SrcNode::new)
            .separated_by(just(Token::Op(Op::Add)))
            .allow_leading())
}

pub fn implied_member_parser() -> impl Parser<(SrcNode<ast::ClassInst>, Vec<(SrcNode<ast::Ident>, SrcNode<ast::Type>)>)> {
    let assoc = nested_parser(
        type_ident_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Op(Op::Eq)))
            .then(type_parser().map_with_span(SrcNode::new))
            .separated_by(just(Token::Comma))
            .allow_trailing(),
        Delimiter::Brace,
        |_| Vec::new(),
    );

    class_inst_parser()
        .map_with_span(SrcNode::new)
        .then(just(Token::With).ignore_then(assoc)
            .or_not()
            .map(|xs| xs.unwrap_or_default()))
}

pub fn generics_parser() -> impl Parser<Vec<(ast::GenericTy, Vec<SrcNode<ast::ImpliedMember>>)>> {
    type_ident_parser()
        .map_with_span(SrcNode::new)
        .then(just(Token::Op(Op::Less)).ignore_then(implied_member_parser()
            .separated_by(just(Token::Op(Op::Add)))
            .allow_leading())
            .or_not())
        .map_with_span(|(name, implied_members), span| (
            ast::GenericTy { name: name.clone() },
            implied_members
                .unwrap_or_default()
                .into_iter()
                .map(move |(class, assoc)| SrcNode::new(ast::ImpliedMember {
                    member: SrcNode::new(ast::Type::Data(name.clone(), Vec::new()), name.span()),
                    class,
                    assoc,
                }, span))
                .collect(),
        ))
        .separated_by(just(Token::Comma))
        .allow_trailing()
}

pub fn where_parser() -> impl Parser<Vec<SrcNode<ast::ImpliedMember>>> {
    let clause = type_parser()
        .map_with_span(SrcNode::new)
        .then_ignore(just(Token::Op(Op::Less)))
        .then(implied_member_parser()
            .separated_by(just(Token::Op(Op::Add)))
            .allow_leading())
        .map_with_span(SrcNode::new);

    just(Token::Where)
        .ignore_then(clause
            .separated_by(just(Token::Comma))
            .allow_trailing())
        .or_not()
        .map(|clauses| clauses
            .into_iter()
            .flat_map(|clauses| clauses.into_iter())
            .flat_map(|clause| {
                let span = clause.span();
                let (ty, classes) = clause.into_inner();
                classes
                    .into_iter()
                    .map(move |(class, assoc)| SrcNode::new(ast::ImpliedMember {
                        member: ty.clone(),
                        class,
                        assoc,
                    }, span))
            })
            .collect())
}

const ITEM_STARTS: [Token; 8] = [
    Token::Data,
    Token::Type,
    Token::Def,
    Token::Class,
    Token::Member,
    Token::For,
    Token::Fn,
    Token::Effect,
];

pub fn data_parser() -> impl Parser<ast::Data> {
    let variant = type_ident_parser()
        .map_with_span(SrcNode::new)
        .then(type_parser()
            .map_with_span(SrcNode::new)
            .or_not())
        .map(|(name, ty)| {
            let name_span = name.span();
            (name, ty.unwrap_or_else(|| SrcNode::new(ast::Type::Tuple(Vec::new()), name_span)))
        })
        .boxed();

    just(Token::Data)
        .ignore_then(type_ident_parser()
            .map_with_span(SrcNode::new))
        .then(generics_parser()
            .then(where_parser())
            .map(|(tys, implied)| ast::Generics::from_tys_and_implied(tys, implied)))
        .then(just(Token::Op(Op::Eq))
            // TODO: Don't use `Result`
            .ignore_then(type_parser().map_with_span(SrcNode::new).map(Err)
                .or(branches(variant).map(Some).or(just(Token::Pipe).to(None)).map(Ok)))
            .or_not())
        .map(|((name, generics), variants)| ast::Data {
            generics,
            variants: variants
                .unwrap_or_else(|| Ok(Some(vec![(name.clone(), SrcNode::new(ast::Type::Tuple(Vec::new()), name.span()))])))
                .unwrap_or_else(|ty| Some(vec![(name.clone(), ty)]))
                .unwrap_or_else(|| Vec::new()),
            name,
        })
        .boxed()
}

pub fn alias_parser() -> impl Parser<ast::Alias> {
    just(Token::Type)
        .ignore_then(type_ident_parser()
            .map_with_span(SrcNode::new))
        .then(generics_parser()
            .then(where_parser())
            .map(|(tys, implied)| ast::Generics::from_tys_and_implied(tys, implied)))
        .then_ignore(just(Token::Op(Op::Eq)))
        .then(type_parser().map_with_span(SrcNode::new))
        .map(|((name, generics), ty)| ast::Alias {
            name,
            generics,
            ty,
        })
        .boxed()
}

pub fn fn_parser() -> impl Parser<ast::Def> {
    let branch = binding_parser()
        .map_with_span(SrcNode::new)
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .map_with_span(SrcNode::new)
        .then_ignore(just(Token::Op(Op::RFlow)))
        .then(expr_parser()
            .map_with_span(SrcNode::new))
        .boxed();

    just(Token::Fn)
        .ignore_then(term_ident_parser()
            .map_with_span(SrcNode::new))
        .then(generics_parser())
        .then(ty_hint_parser())
        .then(where_parser())
        .then_ignore(just(Token::Op(Op::Eq)))
        .then(branches(branch)
            .map_with_span(|branches, span| SrcNode::new(ast::Expr::Func(SrcNode::new(branches, span)), span)))
        .map(|((((name, generics), ty_hint), where_clauses), body)| ast::Def {
            generics: ast::Generics::from_tys_and_implied(generics, where_clauses),
            ty_hint: ty_hint.unwrap_or_else(|| SrcNode::new(ast::Type::Unknown, name.span())),
            name,
            body,
        })
}

pub fn def_parser() -> impl Parser<ast::Def> {
    just(Token::Def)
        .ignore_then(term_ident_parser()
            .map_with_span(SrcNode::new))
        .then(generics_parser())
        .then(ty_hint_parser())
        .then(where_parser())
        .then_ignore(just(Token::Op(Op::Eq)))
        .then(expr_parser().map_with_span(SrcNode::new))
        .map(|((((name, generics), ty_hint), where_clauses), body)| ast::Def {
            generics: ast::Generics::from_tys_and_implied(generics, where_clauses),
            ty_hint: ty_hint.unwrap_or_else(|| SrcNode::new(ast::Type::Unknown, name.span())),
            name,
            body,
        })
        .boxed()
}

pub fn class_parser() -> impl Parser<ast::Class> {
    let value = term_ident_parser()
        .map_with_span(SrcNode::new)
        .then(ty_hint_parser())
        .map(|(name, ty)| ast::ClassItem::Value {
            ty: ty.unwrap_or_else(|| SrcNode::new(ast::Type::Unknown, name.span())),
            name,
        });

    let assoc_type = type_ident_parser()
        .map_with_span(SrcNode::new)
        .then(obligation_parser()
            .or_not()
            .map_with_span(|o, span| SrcNode::new(o.unwrap_or_default(), span)))
        .map(|(name, obligations)| ast::ClassItem::Type {
            obligations,
            name,
        });

    let item = just(Token::Op(Op::RFlow))
        .ignore_then(value.or(assoc_type));

    just(Token::Class)
        .ignore_then(type_ident_parser()
            .map_with_span(SrcNode::new))
        .then(obligation_parser().or_not())
        .then(generics_parser()
            .then(where_parser())
            .map(|(tys, implied)| ast::Generics::from_tys_and_implied(tys, implied)))
        .then(just(Token::Op(Op::Eq))
            .ignore_then(item.repeated())
            .or_not())
        .map(|(((name, obligation), mut generics), items)| ast::Class {
            generics: {
                generics.implied_members.extend(obligation
                    .into_iter()
                    .flatten()
                    .map(|class| {
                        let class_span = class.name.span();
                        // TODO: Horrible
                        SrcNode::new(ast::ImpliedMember {
                            member: SrcNode::new(ast::Type::Data(SrcNode::new(ast::Ident::new("Self"), class_span), Vec::new()), name.span()),
                            class,
                            assoc: Vec::new(),
                        }, class_span)
                    }));
                generics
            },
            name,
            items: items.unwrap_or_default(),
        })
        .boxed()
}

pub fn member_parser() -> impl Parser<ast::Member> {
    let value = term_ident_parser()
        .map_with_span(SrcNode::new)
        .then_ignore(just(Token::Op(Op::Eq)))
        .then(expr_parser()
            .map_with_span(SrcNode::new))
        .map(|(name, val)| ast::MemberItem::Value {
            val,
            name,
        });

    let assoc_type = type_ident_parser()
        .map_with_span(SrcNode::new)
        .then_ignore(just(Token::Op(Op::Eq)))
        .then(type_parser().map_with_span(SrcNode::new))
        .map(|(name, ty)| ast::MemberItem::Type { name, ty });

    let item = just(Token::Op(Op::RFlow))
        .ignore_then(value.or(assoc_type));

    just(Token::For)
        .ignore_then(generics_parser())
        .or_not()
        .then(just(Token::Member)
            .ignore_then(type_parser()
                .map_with_span(SrcNode::new))
            .then_ignore(just(Token::Of))
            .then(class_inst_parser()
                .map_with_span(SrcNode::new))
            .then(where_parser())
            .then(just(Token::Op(Op::Eq))
                .ignore_then(item.repeated())
                .or_not()))
        .map(|(generic_tys, (((member, class), implied), items))| ast::Member {
            generics: ast::Generics::from_tys_and_implied(
                generic_tys.unwrap_or_default(),
                implied,
            ),
            member,
            class,
            items: items.unwrap_or_default(),
        })
        .boxed()
}

pub fn effect_parser() -> impl Parser<ast::Effect> {
    let ty = type_parser()
        .map_with_span(SrcNode::new);

    just(Token::Effect)
        .ignore_then(term_ident_parser().map_with_span(SrcNode::new))
        .then(generics_parser()
            .then(where_parser())
            .map(|(tys, implied)| ast::Generics::from_tys_and_implied(tys, implied)))
        .then_ignore(just(Token::Op(Op::Eq)))
        .then(ty.clone())
        .then_ignore(just(Token::Op(Op::RFlow)))
        .then(ty)
        .map(|(((name, generics), send), recv)| ast::Effect {
            name,
            generics,
            send,
            recv,
        })
        .boxed()
}

pub fn item_parser() -> impl Parser<ast::Item> {
    let attr = recursive(|attr| term_ident_parser()
        .map_with_span(SrcNode::new)
        .then(nested_parser(
                attr
                        .separated_by(just(Token::Comma))
                        .allow_trailing(),
                Delimiter::Paren,
                |_| Vec::new(),
            )
            .or_not())
        .map(|(name, args)| ast::Attr { name, args })
        .map_with_span(SrcNode::new));

    let attrs = just(Token::Dollar)
        .ignore_then(nested_parser(
            attr
                .separated_by(just(Token::Comma))
                .allow_trailing(),
            Delimiter::Brack,
            |_| Vec::new(),
        ))
        .repeated()
        .flatten();

    let item = def_parser().map(ast::ItemKind::Def).labelled("definition")
        .or(fn_parser().map(ast::ItemKind::Def).labelled("function"))
        .or(data_parser().map(ast::ItemKind::Data).labelled("data type"))
        .or(alias_parser().map(ast::ItemKind::Alias).labelled("type alias"))
        .or(class_parser().map(ast::ItemKind::Class).labelled("class"))
        .or(member_parser().map(ast::ItemKind::Member).labelled("class member"))
        .or(effect_parser().map(ast::ItemKind::Effect).labelled("effect"));

    let tail = one_of::<_, _, Error>(ITEM_STARTS)
        .ignored()
        .or(just(Token::Dollar).ignored())
        .or(end());

    attrs
        .then(item)
        .map(|(attrs, kind)| ast::Item { attrs, kind })
        .map_with_span(|item, span| (item, span))
        .then(tail.rewind().map(Ok).map(Some).or_else(|e| Ok(Some(Err(e)))))
        .validate(|((item, span), mut r), _, emit| {
            if let Some(Err(e)) = r.take() {
                emit(e.while_parsing(span, "item"));
            }
            item
        })
}

pub fn module_parser() -> impl Parser<ast::Module> {
    let imports = just(Token::Import)
        .ignore_then(select! { Token::Str(path) => path }.map_with_span(SrcNode::new))
        .repeated();

    imports
        .then(item_parser()
            .map(Some)
            .recover_with(skip_until(ITEM_STARTS, |_| None).skip_start())
            .repeated())
        .then_ignore(end())
        .map(|(imports, items)| ast::Module {
            imports,
            items: items.into_iter().flatten().collect(),
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser;

    #[test]
    fn simple() {
        let code = r#"
            4 * 5 + (3 - 2) / foo(3, 4)
        "#;
        let len = code.chars().count();

        let span = |i| Span::new(SrcId::empty(), i..i + 1);

        let tokens = token::lexer()
            .parse(chumsky::Stream::from_iter(
                span(len),
                code.chars().enumerate().map(|(i, c)| (c, span(i))),
            ))
            .unwrap();

        let res = expr_parser()
            .then_ignore(end())
            .parse(chumsky::Stream::from_iter(
                span(len),
                tokens.into_iter(),
            ))
            .unwrap();

        assert_eq!(
            res,
            ast::Expr::Binary(
                SrcNode::new(ast::BinaryOp::Add, Span::empty()),
                SrcNode::new(ast::Expr::Binary(
                    SrcNode::new(ast::BinaryOp::Mul, Span::empty()),
                    SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(4)), Span::empty()),
                    SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(5)), Span::empty()),
                ), Span::empty()),
                SrcNode::new(ast::Expr::Binary(
                    SrcNode::new(ast::BinaryOp::Div, Span::empty()),
                    SrcNode::new(ast::Expr::Binary(
                        SrcNode::new(ast::BinaryOp::Sub, Span::empty()),
                        SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(3)), Span::empty()),
                        SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(2)), Span::empty()),
                    ), Span::empty()),
                    SrcNode::new(ast::Expr::Apply(
                        SrcNode::new(ast::Expr::Apply(
                            SrcNode::new(ast::Expr::Local(ast::Ident::new("foo")), Span::empty()),
                            SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(3)), Span::empty()),
                        ), Span::empty()),
                        SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(4)), Span::empty()),
                    ), Span::empty()),
                ), Span::empty()),
            ),
            "{:#?}", res,
        );
    }
}
