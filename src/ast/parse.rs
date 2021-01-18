// use parze::{prelude::*, Error as ParzeError, Span as ParzeSpan};
use lingo::{prelude::*, Span as LingoSpan};
use crate::{
    util::{Span, SrcNode},
    ErrorCode, Error,
};
use super::{
    lex::{Token, Op, Delimiter},
    SrcId,
    Ident, SrcStr, Expr, Type, Item, PathBase, Literal, Pat, Binding, UnaryOp, BinaryOp, MatchArm,
};
use std::{ops::Range, cmp::Ordering, marker::PhantomData};

pub enum ParseError {
    Unexpected(Token, Span, Option<Token>),
    UnexpectedEnd,
}

struct ParseContext<'a>(PhantomData<&'a ()>);

impl<'a> Context for ParseContext<'a> {
    type Token = &'a Token;
    type Span = Span;
    type Error = ParseError;
}

impl LingoSpan for Span {
    fn start() -> Self {
        Self::none()
    }

    fn end() -> Self {
        Self::none()
    }

    fn union(self, other: Self) -> Self {
        Span::union(self, other)
    }

    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Span::partial_cmp(self, other)
    }
}

impl<'a> lingo::Error<ParseContext<'a>> for ParseError {
    type Item = Token;

    fn span(&self) -> Option<<ParseContext as Context>::Span> {
        Some(match self {
            ParseError::Unexpected(_, span, _) => span.clone(),
            ParseError::UnexpectedEnd => Span::none(),
        })
    }

    fn unexpected_end() -> Self { ParseError::UnexpectedEnd }

    fn unexpected(found: Self::Item, span: <ParseContext as Context>::Span, expected: Option<Self::Item>) -> Self {
        ParseError::Unexpected(found, span, expected)
    }
}

/*
pub enum ParseError<'a> {
    Unexpected(&'a SrcNode<Token>, Span),
    UnexpectedEnd,
}

impl<'a> ParzeError<&'a SrcNode<Token>> for ParseError<'a> {
    type Thing = Token;
    type Span = Span;

    fn position(&self) -> Option<usize> {
        match self {
            ParseError::Unexpected(_, x) => Some(*x),
            _ => None,
        }
    }

    fn unexpected_end() -> Self {
        ParseError::UnexpectedEnd
    }

    fn unexpected(span: Self::Span, f: &'a SrcNode<Token>) -> Self {
        ParseError::Unexpected(f, span)
    }
}

impl ParzeSpan for Span {
    fn none() -> Self { Self::none() }
    fn union(self, other: Self) -> Self { self.union(other) }

    // fn from_tokens<I: DoubleEndedIterator<Item=(usize, &'a SrcNode<Token>)> + ExactSizeIterator<Item=(usize, &'a SrcNode<Token>)>>(mut tokens: I) -> Self {
    //     let first = tokens.next();
    //     let last = tokens.rev().next();
    //     first
    //         .zip_with(last, |(_, first), (_, last)| first.span().union(last.span()))
    //         .unwrap_or_else(Span::none)
    // }
}

#[derive(Copy, Clone)]
pub struct TokenStream<'a>(&'a [SrcNode<Token>]);

impl<'a> Stream for TokenStream<'a> {
    type Token = &'a SrcNode<Token>;
    type Span = Span;

    fn next(self) -> (Option<(Self::Token, Self::Span)>, Self) {
        match self.0 {
            [head, tail @ ..] => (Some((head, head.span())), tail),
            _ => (None, self),
        }
    }
}

trait AstParser<'a, O> = Parser<&'a SrcNode<Token>, O, Span=<ParseError<'a> as ParzeError<&'a SrcNode<Token>>>::Span, Stream=TokenStream<'a>, Error=ParseError<'a>>;
*/

fn ident_parser<'a>() -> impl Parser<ParseContext<'a>, Ident> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Ident(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
    .boxed()
}

fn intrinsic_parser<'a>() -> impl Parser<ParseContext<'a>, Ident> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Intrinsic(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn type_name_parser<'a>() -> impl Parser<ParseContext<'a>, Ident> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::TypeName(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn path_base_parser<'a>() -> impl Parser<ParseContext<'a>, PathBase> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Root => Ok(PathBase::Root),
        Token::This => Ok(PathBase::This),
        Token::Super => Ok(PathBase::Super),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn global_parser<'a>(src_id: SrcId) -> impl Parser<ParseContext<'a>, SrcNode<Item>> + Clone {
    path_base_parser()
        .or_not()
        .map(|base| base.unwrap_or(PathBase::This))
        .map_with_span(move |base, span| SrcNode::new(base, span))
        .then(ident_parser()
            .map_with_span(move |ident, span| SrcNode::new(ident, span))
            .once_or_more())
        .map_with_span(move |(base, mut path), span| {
            let name = path.pop().unwrap();

            SrcNode::new(Item {
                name,
                base: Some(base),
                path,
            }, span)
        })
}

fn data_name_parser<'a>(src_id: SrcId) -> impl Parser<ParseContext<'a>, SrcNode<Item>> + Clone {
    path_base_parser()
        .or_not()
        .map(|base| base.unwrap_or(PathBase::This))
        .map_with_span(move |base, span| SrcNode::new(base, span))
        .then(ident_parser()
            .map_with_span(move |ident, span| SrcNode::new(ident, span))
            .repeated())
        .then(type_name_parser()
            .map_with_span(move |ident, span| SrcNode::new(ident, span)))
        .map_with_span(move |((base, path), name), span| {
            SrcNode::new(Item {
                name,
                base: Some(base),
                path,
            }, span)
        })
}

fn natural_parser<'a>() -> impl Parser<ParseContext<'a>, SrcStr> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Nat(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn integer_parser<'a>() -> impl Parser<ParseContext<'a>, SrcStr> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Int(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn number_parser<'a>() -> impl Parser<ParseContext<'a>, SrcStr> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Num(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn char_parser<'a>() -> impl Parser<ParseContext<'a>, char> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Char(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn string_parser<'a>() -> impl Parser<ParseContext<'a>, SrcStr> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::String(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn boolean_parser<'a>() -> impl Parser<ParseContext<'a>, bool> + Clone {
    permit_map(|tok: &Token, span| match tok {
        Token::Boolean(x) => Ok(*x),
        _ => Err(ParseError::Unexpected(tok.clone(), span, None)),
    })
}

fn nested_parser<'a, O>(inner: impl Parser<ParseContext<'a>, O> + Clone, delim: Delimiter) -> impl Parser<ParseContext<'a>, O> + Clone {
    nested(inner.padded_by(end()), move |&tok: &&Token, span| {
        if let Token::Tree(d, tokens) = tok {
            if *d == delim {
                Ok(Stream::from_iter(tokens.iter().map(|tok| (tok.inner(), tok.span()))))
            } else {
                Err(ParseError::Unexpected(tok.clone(), span, None))
            }
        } else {
            Err(ParseError::Unexpected(tok.clone(), span, None))
        }
    })
}

fn type_parser<'a>(src_id: SrcId) -> impl Parser<ParseContext<'a>, SrcNode<Type>> + Clone {
    recursive(|ty| {
        let atom = {
            let ty = ty.clone();

            recursive(move |atom| {
                let data = data_name_parser(src_id)
                    .map_with_span(move |data_name, span| SrcNode::new(Type::Data(data_name, Vec::new()), span));

                let list = nested_parser(
                    ty.clone(),
                    Delimiter::Brack,
                )
                    .map(|ty| Type::List(ty));

                let tuple = nested_parser(
                    ty.clone()
                        .separated_by(just(Token::Comma))
                        .padded_by(just(Token::Comma).or_not()),
                    Delimiter::Paren,
                )
                    .map(|tys| Type::Tuple(tys));

                let record = nested_parser(
                    ident_parser()
                        .map_with_span(move |ty, span| SrcNode::new(ty, span))
                        .padded_by(just(Token::Colon))
                        .then(ty.clone())
                        .separated_by(just(Token::Comma))
                        .padded_by(just(Token::Comma).or_not()),
                    Delimiter::Brace,
                )
                    .map(|tys| Type::Record(tys));

                let unknown = just(Token::QuestionMark)
                    .map(|_| Type::Unknown);

                let paren_ty = nested_parser(
                    ty.clone(),
                    Delimiter::Paren,
                );

                let paren_ty_list = nested_parser(
                    ty
                        .clone()
                        .separated_by(just(Token::Comma))
                        .padded_by(just(Token::Comma).or_not()),
                    Delimiter::Paren,
                );

                let atom = paren_ty
                    .or(data)
                    .or(list
                        .or(tuple)
                        .or(record)
                        .or(unknown)
                        .map_with_span(move |ty, span| SrcNode::new(ty, span)));

                atom
            })
        };

        let data = data_name_parser(src_id)
            .then(atom.clone().repeated())
            .map(|(data, params)| Type::Data(data, params))
            .map_with_span(move |ty, span| SrcNode::new(ty, span))
            .or(atom);

        data.clone()
            .then(just(Token::RArrow).padding_for(ty.clone()).repeated())
            .reduce_left(|i: SrcNode<Type>, o: SrcNode<Type>| {
                let span = i.span().union(o.span());
                SrcNode::new(Type::Func(i, o), span)
            })
            .or(data)
    })
}

// pub fn parse_type(src_id: SrcId, tokens: &[SrcNode<Token>]) -> (Option<SrcNode<Type>>, Vec<Error>) {
//     let (ty, errs) = type_parser().padded_by(end()).parse(tokens);
//     (ty, errs
//         .into_iter()
//         .map(move |e| match e {
//             ParseError::UnexpectedEnd => Error::new(
//                 ErrorCode::UnexpectedEnd,
//                 Span::none().with_src(src_id),
//                 format!("Unexpected end of input"),
//             ),
//             ParseError::Unexpected(token, _) => Error::new(
//                 ErrorCode::UnexpectedToken,
//                 token.span(),
//                 format!("Unexpected token `{}`", token.inner()),
//             )
//                 .with_primary(token.span(), None),
//         })
//         .collect())
// }

fn litr_parser<'a>() -> impl Parser<ParseContext<'a>, Literal> + Clone {
    let natural = natural_parser().map(|x| Literal::Nat(x));
    let integer = integer_parser().map(|x| Literal::Int(x));
    let number = number_parser().map(|x| Literal::Num(x));
    let character = char_parser().map(|x| Literal::Char(x));
    let string = string_parser().map(|x| Literal::Str(x));
    let boolean = boolean_parser().map(|x| Literal::Bool(x));

    natural
        .or(integer)
        .or(number)
        .or(character)
        .or(string)
        .or(boolean)
}

fn binding_parser<'a>(src_id: SrcId) -> impl Parser<ParseContext<'a>, SrcNode<Binding>> + Clone {
    recursive(move |binding| {
        let binding2 = binding.clone();
        let pat = recursive(move |pat| {
            let binding = binding2;

            let wildcard = just(Token::Wildcard)
                .map_with_span(move |pat, span| SrcNode::new(Pat::Wildcard, span));

            let litr = litr_parser()
                .map_with_span(move |pat, span| SrcNode::new(Pat::Literal(pat), span));

            let tuple = nested_parser(
                binding.clone()
                    .separated_by(just(Token::Comma))
                    .padded_by(just(Token::Comma).or_not()),
                Delimiter::Paren,
            )
                .map_with_span(move |items, span| SrcNode::new(Pat::Tuple(items), span));

            let record = nested_parser(
                ident_parser()
                    .map_with_span(move |field, span| SrcNode::new(field, span))
                    .then(just(Token::Colon)
                        .padding_for(binding.clone())
                        .or_not())
                    .map_with_span(move |(field, binding), span| {
                        let binding = binding.unwrap_or_else(|| SrcNode::new(Binding {
                            pat: SrcNode::new(Pat::Wildcard, field.span()),
                            binding: Some(field.clone()),
                            ty: SrcNode::new(Type::Unknown, span),
                        }, span));

                        (field, binding)
                    })
                    .separated_by(just(Token::Comma))
                    .padded_by(just(Token::Comma).or_not()),
                Delimiter::Brace,
            )
                .map_with_span(move |fields, span| SrcNode::new(Pat::Record(fields), span))
                .boxed();

            let list = nested_parser(
                binding.clone()
                    .separated_by(just(Token::Comma))
                    .padded_by(just(Token::Comma).or_not()),
                Delimiter::Brack,
            )
                .map_with_span(move |items, span| SrcNode::new(Pat::List(items), span))
                .boxed();

            let list_front = nested_parser(
                binding.clone().padded_by(just(Token::Comma)).repeated()
                    .then(ident_parser()
                        .padded_by(just(Token::Colon))
                        .map_with_span(move |ident, span| SrcNode::new(ident, span))
                        .or_not())
                    .padded_by(just(Token::Op(Op::Ellipsis))),
                Delimiter::Brack,
            )
                .map_with_span(move |(items, tail), span| SrcNode::new(Pat::ListFront(items, tail), span))
                .boxed();

            let deconstruct = data_name_parser(src_id)
                .then(binding.or_not())
                .map_with_span(move |(data, inner), span| {
                    let inner = inner.unwrap_or_else(|| SrcNode::new(Binding {
                        pat: SrcNode::new(Pat::Tuple(Vec::new()), data.span()),
                        binding: None,
                        ty: SrcNode::new(Type::Unknown, data.span()),
                    }, data.span()));

                    SrcNode::new(Pat::Deconstruct(data, inner), span)
                })
                .boxed();

            wildcard
                .or(litr)
                .or(deconstruct)
                .or(tuple)
                .or(record)
                .or(list)
                .or(list_front)
        });

        let ty_hint = just(Token::Separator)
            .padding_for(type_parser(src_id))
            .or_not()
            .map(|ty| ty.unwrap_or_else(|| SrcNode::new(Type::Unknown, Span::none())));

        // Bound pattern
        ident_parser()
            .map_with_span(move |ident, span| SrcNode::new(ident, span))
            .padded_by(just(Token::Colon))
            .then(pat.clone())
            .map(|(binding, pat)| (pat, Some(binding)))
            // Unbound pattern
            .or(pat.map(|pat| (pat, None)))
            // Ident
            .or(ident_parser().map_with_span(move |name, span| (
                SrcNode::new(Pat::Wildcard, span),
                Some(SrcNode::new(name, span)),
            )))
            .then(ty_hint.or_not())
            .map_with_span(move |((pat, binding), ty), span| SrcNode::new(Binding {
                pat,
                binding,
                ty: ty.unwrap_or_else(move || SrcNode::new(Type::Unknown, span)),
            }, span))
    })
}

fn expr_parser<'a>(src_id: SrcId) -> impl Parser<ParseContext<'a>, SrcNode<Expr>> + Clone {
    recursive(move |expr| {
        let brack_expr_list = nested_parser(
            expr.clone().separated_by(just(Token::Comma)).padded_by(just(Token::Comma).or_not()),
            Delimiter::Brack,
        );

        let paren_expr_list = nested_parser(
            expr.clone().separated_by(just(Token::Comma)).padded_by(just(Token::Comma).or_not()),
            Delimiter::Paren,
        );

        let brace_field_list = nested_parser(
            ident_parser()
                .map_with_span(move |ident, span| SrcNode::new(ident, span))
                .then(just(Token::Colon)
                    .padding_for(expr.clone())
                    .or_not())
                .map(|(field, val)| (field.clone(), val.unwrap_or_else(|| SrcNode::new(Expr::Item(Item::local(field.clone())), field.span()))))
                .separated_by(just(Token::Comma))
                .padded_by(just(Token::Comma).or_not()),
            Delimiter::Brace,
        );

        let binding = binding_parser(src_id);

        let arm_list = nested_parser(
            just(Token::Pipe)
                .or_not()
                .padding_for(binding.clone()
                    .padded_by(just(Token::RMap))
                    .then(expr.clone())
                    .map(|(binding, body)| MatchArm {
                        binding,
                        body,
                    })
                    .separated_by(just(Token::Pipe))
                    .padded_by(just(Token::Comma).or_not())),
            Delimiter::Brace,
        );

        let litr = litr_parser()
            .map_with_span(move |litr, span| SrcNode::new(Expr::Literal(litr), span));

        let ident = ident_parser()
            .map_with_span(move |ident, span| SrcNode::new(Expr::Item(Item::local(SrcNode::new(ident, span))), span));

        let intrinsic = intrinsic_parser()
            .map_with_span(move |ident, span| SrcNode::new(ident, span))
            .then(paren_expr_list.clone().or_not())
            .map_with_span(move |(ident, args), span| SrcNode::new(Expr::Intrinsic(ident, args.unwrap_or(Vec::new())), span));

        let constructor = data_name_parser(src_id)
            .then(expr.clone().or_not())
            .map_with_span(move |(data, expr), span| SrcNode::new(
                Expr::Construct(data, expr.unwrap_or_else(|| SrcNode::new(Expr::Tuple(Vec::new()), Span::none()))),
                span,
            ));

        // let do_statement = binding.clone()
        //     .padded_by(just(Token::LArrow))
        //     .then(expr.clone())
        //     .map(|((binding, ty), expr)| DoStatement::Bind(binding, ty, expr))
        //     .or(expr.clone().map(DoStatement::Exec))
        //     .or(just(Token::Return).padding_for(expr.clone()).map(DoStatement::Return));

        let atom = litr
            .or(ident)
            .or(intrinsic)
            .or(constructor)
            // Parenthesised expression
            .or(nested_parser(expr.clone(), Delimiter::Paren))
            // Lists
            .or(brack_expr_list
                .map_with_span(move |items, span| SrcNode::new(Expr::List(items), span)))
            // Tuples
            .or(paren_expr_list
                .clone()
                .map_with_span(move |items, span| SrcNode::new(Expr::Tuple(items), span)))
            // Records
            .or(brace_field_list
                .clone()
                .map_with_span(move |fields, span| SrcNode::new(Expr::Record(fields), span)))
            // Let
            .or(just(Token::Let)
                .padding_for(binding.clone())
                .padded_by(just(Token::Op(Op::Eq)))
                .then(expr.clone())
                .map_with_span(move |(pat, expr), span| ((pat, expr), span))
                .padded_by(just(Token::In))
                .then(expr.clone())
                .map(move |(((pat, expr), span), then)| SrcNode::new(Expr::Let(pat, expr, then), span)))
            // If
            .or(just(Token::If)
                .padding_for(expr.clone())
                .padded_by(just(Token::Then))
                .then(expr.clone())
                .padded_by(just(Token::Else))
                .then(expr.clone())
                .map_with_span(move |((pred, a), b), span| SrcNode::new(Expr::If(pred, a, b), span)))
            // Match
            .or(just(Token::Match)
                .padding_for(expr.clone())
                .then(arm_list)
                .map_with_span(move |(pred, arms), span| SrcNode::new(Expr::Match(pred, arms), span)))
            // .or(just(Token::Do)
            //     .padding_for(ident_parser())
            //     .then(nested_parser(
            //             do_statement
            //                 .clone()
            //                 .then(just(Token::Semicolon).padding_for(do_statement.separated_by(just(Token::Semicolon)).padded_by(just(Token::Comma).or_not())).or_not())
            //             .map(|(x, xs)| {
            //                 let mut x = vec![x];
            //                 if let Some(mut xs) = xs {
            //                     x.append(&mut xs);
            //                 }
            //                 x
            //             }),
            //         Delimiter::Brace,
            //     ))
            //     .map(|(prefix, mut xs)| {
            //         let x = xs.pop().unwrap();
            //         (xs, (prefix, match x {
            //             DoStatement::Exec(expr) => expr,
            //             DoStatement::Return(expr) => {
            //                 let make = SrcNode::new(Expr::Path(Path(vec![LocalIntern::new(format!("{}_make", prefix))])), Span::none());
            //                 SrcNode::new(Expr::Apply(make, expr), Span::none())
            //             },
            //             DoStatement::Bind(_, _, expr) => expr,
            //         }))
            //     })
            //     .reduce_right(|l, (prefix, r)| match l {
            //         DoStatement::Exec(l) => {
            //             let next = SrcNode::new(Expr::Path(Path(vec![LocalIntern::new(format!("{}_next", prefix))])), Span::none());
            //             (prefix, SrcNode::new(Expr::Apply(SrcNode::new(Expr::Apply(next, r), Span::none()), l), Span::none()))
            //         },
            //         DoStatement::Return(l) => {
            //             let next = SrcNode::new(Expr::Path(Path(vec![LocalIntern::new(format!("{}_next", prefix))])), Span::none());
            //             let make = SrcNode::new(Expr::Path(Path(vec![LocalIntern::new(format!("{}_make", prefix))])), Span::none());
            //             (prefix, SrcNode::new(Expr::Apply(SrcNode::new(Expr::Apply(next, SrcNode::new(Expr::Apply(make, r), Span::none())), Span::none()), l), Span::none()))
            //         },
            //         DoStatement::Bind(b, ty, l) => {
            //             let bind = SrcNode::new(Expr::Path(Path(vec![LocalIntern::new(format!("{}_bind", prefix))])), Span::none());
            //             (prefix, SrcNode::new(Expr::Apply(SrcNode::new(Expr::Apply(bind, SrcNode::new(Expr::Func(b, ty, r), Span::none())), Span::none()), l), Span::none()))
            //         },
            //     })
            //     .map(|(_, expr)| expr))
            .boxed();

        let application = atom
            .then(paren_expr_list.repeated())
            .reduce_left(|f: SrcNode<Expr>, args: Vec<SrcNode<Expr>>| args
                .into_iter()
                .fold(f, |f, arg| {
                    let span = f.span().union(arg.span());
                    SrcNode::new(Expr::Apply(f, arg), span)
                }))
            .boxed();

        let access = application.clone()
            .then(just(Token::Dot)
                .padding_for(ident_parser()
                    .map_with_span(move |field, span| SrcNode::new(field, span)))
            .repeated())
            .reduce_left(|expr: SrcNode<Expr>, field: SrcNode<Ident>| {
                let span = expr.span().union(field.span());
                SrcNode::new(Expr::Access(expr, field), span)
            })
            .boxed();

        let infix = access.clone()
            .then(just(Token::Colon).padding_for(access).repeated())
            .reduce_left(|arg: SrcNode<Expr>, f: SrcNode<Expr>| {
                let span = f.span().union(arg.span());
                SrcNode::new(Expr::Apply(f, arg), span)
            })
            .boxed();

        let coerce = just(Token::Ampersand)
            .map_with_span(move |_, span| span)
            .repeated()
            .then(infix)
            .reduce_right(|span, expr: SrcNode<Expr>| {
                let span = span.union(expr.span());
                SrcNode::new(Expr::Coerce(expr), span)
            })
            .boxed();

        let unary = just(Token::Op(Op::Sub)).to(UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(UnaryOp::Not))
            .map_with_span(move |op, span| SrcNode::new(op, span))
            .repeated()
            .then(coerce)
            .reduce_right(|op: SrcNode<UnaryOp>, expr: SrcNode<Expr>| {
                let span = op.span().union(expr.span());
                SrcNode::new(Expr::Unary(op, expr), span)
            })
            .boxed();

        let product_op = just(Token::Op(Op::Mul)).to(BinaryOp::Mul)
            .or(just(Token::Op(Op::Div)).to(BinaryOp::Div))
            .or(just(Token::Op(Op::Rem)).to(BinaryOp::Rem))
            .map_with_span(move |op, span| SrcNode::new(op, span));
        let product = unary.clone()
            .then(product_op.then(unary).repeated())
            .reduce_left(|a: SrcNode<Expr>, (op, b): (SrcNode<BinaryOp>, SrcNode<Expr>)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let sum_op = just(Token::Op(Op::Add)).to(BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(BinaryOp::Sub))
            .map_with_span(move |op, span| SrcNode::new(op, span));
        let sum = product.clone()
            .then(sum_op.then(product).repeated())
            .reduce_left(|a: SrcNode<Expr>, (op, b): (SrcNode<BinaryOp>, SrcNode<Expr>)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let join_op = just(Token::Op(Op::Join)).to(BinaryOp::Join)
            .map_with_span(move |op, span| SrcNode::new(op, span));
        let join = sum.clone()
            .then(join_op.then(sum).repeated())
            .reduce_left(|a: SrcNode<Expr>, (op, b): (SrcNode<BinaryOp>, SrcNode<Expr>)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let comparison_op = just(Token::Op(Op::Eq)).to(BinaryOp::Eq)
            .or(just(Token::Op(Op::NotEq)).to(BinaryOp::NotEq))
            .or(just(Token::Op(Op::Less)).to(BinaryOp::Less))
            .or(just(Token::Op(Op::More)).to(BinaryOp::More))
            .or(just(Token::Op(Op::LessEq)).to(BinaryOp::LessEq))
            .or(just(Token::Op(Op::MoreEq)).to(BinaryOp::MoreEq))
            .map_with_span(move |op, span| SrcNode::new(op, span));
        let comparison = join.clone()
            .then(comparison_op.then(join).repeated())
            .reduce_left(|a: SrcNode<Expr>, (op, b): (SrcNode<BinaryOp>, SrcNode<Expr>)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let logical_op = just(Token::Op(Op::And)).to(BinaryOp::And)
            .or(just(Token::Op(Op::Or)).to(BinaryOp::Or))
            .map_with_span(move |op, span| SrcNode::new(op, span));
        let logical = comparison.clone()
            .then(logical_op.then(comparison).repeated())
            .reduce_left(|a: SrcNode<Expr>, (op, b): (SrcNode<BinaryOp>, SrcNode<Expr>)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let update = logical.clone()
            .then(just(Token::With)
                .padding_for(brace_field_list)
                .or_not())
            .map(|(expr, withs)| (expr, withs.unwrap_or_else(|| Vec::new())))
            .reduce_left(|expr: SrcNode<Expr>, (field, value): (SrcNode<Ident>, SrcNode<Expr>)| {
                let span = expr.span().union(field.span()).union(value.span());
                SrcNode::new(Expr::Update(expr, field, value), span)
            })
            .boxed();

        let func = just(Token::Pipe)
            .padding_for(binding.separated_by(just(Token::Comma)).padded_by(just(Token::Comma).or_not()))
            .padded_by(just(Token::Pipe))
            .then(expr)
            .reduce_right(|param: SrcNode<Binding>, body: SrcNode<Expr>| {
                let span = param
                    .span()
                    .union(body.span());
                SrcNode::new(Expr::Func(param, body), span)
            })
            .boxed();

        func
            .or(update)
    })
}

pub fn parse_expr(src_id: SrcId, tokens: &[SrcNode<Token>]) -> (Option<SrcNode<Expr>>, Vec<Error>) {
    let (ty, errs) = expr_parser(src_id)
        .padded_by(end())
        .parse(Stream::from_iter(tokens.iter().map(|tok| (tok.inner(), tok.span()))));
    (ty, errs
        .into_iter()
        .map(move |e| match e {
            ParseError::UnexpectedEnd => Error::new(
                ErrorCode::UnexpectedEnd,
                Span::none().with_src(src_id),
                format!("Unexpected end of input"),
            ),
            ParseError::Unexpected(token, span, expected) => Error::new(
                ErrorCode::UnexpectedToken,
                span,
                match expected {
                    Some(expected) => format!("Unexpected token `{}`, expected `{}`", token, expected),
                    None => format!("Unexpected token `{}`", token),
                },
            )
                .with_primary(span, None),
        })
        .collect())
}
