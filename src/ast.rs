use std::fmt;
use internment::LocalIntern;
use parze::prelude::*;
use crate::{
    node,
    lex::{Token, Delimiter, Op},
    error::Error,
    node::SrcNode,
    src::Span,
};

type Ident = LocalIntern<String>;

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
}

#[derive(Clone)]
pub struct Path(Vec<Ident>); // Always at least one element

impl Path {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn parts(&self) -> &[Ident] {
        &self.0
    }

    pub fn base(&self) -> Ident {
        *self.0.last().expect("Path must have a base")
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for part in self.0.iter() {
            write!(f, "::{:?}", part)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,

    Not,

    Head,
    Tail,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::Head => write!(f, "<:"),
            UnaryOp::Tail => write!(f, ":>"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eq,
    NotEq,
    Less,
    More,
    LessEq,
    MoreEq,

    And,
    Or,

    Join,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),
            BinaryOp::Eq => write!(f, "="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::More => write!(f, ">"),
            BinaryOp::LessEq => write!(f, "<="),
            BinaryOp::MoreEq => write!(f, ">="),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Join => write!(f, "++"),
        }
    }
}

#[derive(Debug)]
pub enum Pat {
    Wildcard,
    Literal(Literal),
    Inner(SrcNode<Binding>),
    List(Vec<SrcNode<Binding>>),
    ListFront(Vec<SrcNode<Binding>>, Option<SrcNode<Ident>>),
    Tuple(Vec<SrcNode<Binding>>),
}

#[derive(Debug)]
pub struct Binding {
    pub pat: SrcNode<Pat>,
    pub binding: Option<SrcNode<Ident>>,
}

#[derive(Clone, Debug)]
pub enum Type {
    Unknown,
    List(SrcNode<Self>),
    Tuple(Vec<SrcNode<Self>>),
    Func(SrcNode<Self>, SrcNode<Self>),
    Data(SrcNode<Ident>, Vec<SrcNode<Self>>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Path(Path),
    Unary(SrcNode<UnaryOp>, SrcNode<Self>),
    Binary(SrcNode<BinaryOp>, SrcNode<Self>, SrcNode<Self>),
    If(SrcNode<Self>, SrcNode<Self>, SrcNode<Self>),
    Match(SrcNode<Self>, Vec<((SrcNode<Binding>, Option<SrcNode<Type>>), SrcNode<Self>)>),
    Func(SrcNode<Binding>, Option<SrcNode<Type>>, SrcNode<Self>),
    Apply(SrcNode<Self>, SrcNode<Self>),
    Let(SrcNode<Binding>, Option<SrcNode<Type>>, SrcNode<Self>, SrcNode<Self>),
    List(Vec<SrcNode<Self>>),
    Tuple(Vec<SrcNode<Self>>),
}

fn ident_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=Ident>, Error> {
    permit_map(|token: node::Node<_>| match &*token {
        Token::Ident(x) => Some(*x),
        _ => None,
    })
}

fn number_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=f64>, Error> {
    permit_map(|token: node::Node<_>| match &*token {
        Token::Number(x) => Some(x.parse().unwrap()),
        _ => None,
    })
}

fn string_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=LocalIntern<String>>, Error> {
    permit_map(|token: node::Node<_>| match &*token {
        Token::String(x) => Some(*x),
        _ => None,
    })
}

fn nested_parser<'a, O: 'a>(
    inner: Parser<impl Pattern<Error, Input=node::Node<Token>, Output=O> + 'a, Error>,
    delim: Delimiter,
) -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=O> + 'a, Error> {
    nested_parse(move |token: node::Node<Token>| {
        if let Token::Tree(d, tokens) = token.into_inner() {
            if d == delim {
                Some((inner.clone().padded_by(end()), tokens))
            } else {
                None
            }
        } else {
            None
        }
    })
}

fn type_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<Type>>, Error> {
    recursive(|ty| {
        let ty = ty.link();

        let atom = {
            let ty = ty.clone();

            recursive(move |atom| {
                let atom = atom.link();

                let ident = ident_parser()
                    .map_with_span(|ident, span| SrcNode::new(Type::Data(SrcNode::new(ident, span), Vec::new()), span));

                let list = nested_parser(
                    ty.clone(),
                    Delimiter::Brack,
                )
                    .map(|ty| Type::List(ty));

                let tuple = nested_parser(
                    ty.clone().separated_by(just(Token::Comma)),
                    Delimiter::Paren,
                )
                    .map(|ty| Type::Tuple(ty));

                let unknown = just(Token::QuestionMark)
                    .map(|_| Type::Unknown);

                let paren_ty = nested_parser(
                    ty.clone(),
                    Delimiter::Paren,
                );

                let paren_ty_list = nested_parser(
                    ty
                        .clone()
                        .separated_by(just(Token::Comma)),
                    Delimiter::Paren,
                );

                let atom = paren_ty
                    .or(ident)
                    .or(list
                        .or(tuple)
                        .or(unknown)
                        .map_with_span(|ty, span| SrcNode::new(ty, span)));

                atom
            })
        };

        let data = ident_parser()
            .map_with_span(|name, span| SrcNode::new(name, span))
            .then(atom.clone().repeated())
            .map(|(data, params)| Type::Data(data, params))
            .map_with_span(|ty, span| SrcNode::new(ty, span))
            .or(atom);

        data.clone()
            .then(just(Token::RArrow).padding_for(ty.clone()).repeated())
            .reduce_left(|i, o| {
                let span = i.span().union(o.span());
                SrcNode::new(Type::Func(i, o), span)
            })
            .or(data)
    })
}

fn binding_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<Binding>>, Error> {
    recursive(|binding| {
        let binding = binding.link();

        let litr_pat = number_parser().map(|x| Pat::Literal(Literal::Number(x)))
            .or(just(Token::Boolean(true)).map(|_| Pat::Literal(Literal::Boolean(true))))
            .or(just(Token::Boolean(false)).map(|_| Pat::Literal(Literal::Boolean(false))))
            .or(string_parser().map(|x| Pat::Literal(Literal::String(x))))
            .map_with_span(|pat, span| SrcNode::new(pat, span));

        let tuple_pat = nested_parser(
            binding.clone().separated_by(just(Token::Comma)),
            Delimiter::Paren,
        )
            .map_with_span(|items, span| SrcNode::new(Pat::Tuple(items), span));

        let list_pat = nested_parser(
            binding.clone().separated_by(just(Token::Comma)),
            Delimiter::Brack,
        )
            .map_with_span(|items, span| SrcNode::new(Pat::List(items), span));

        let list_front_pat = nested_parser(
            binding.clone().padded_by(just(Token::Comma)).repeated()
                .padded_by(just(Token::Op(Op::Ellipsis)))
                .then(ident_parser().map_with_span(|ident, span| SrcNode::new(ident, span)).or_not()),
            Delimiter::Brack,
        )
            .map_with_span(|(items, tail), span| SrcNode::new(Pat::ListFront(items, tail), span));

        let free_binding = ident_parser()
            .map_with_span(|ident, span| Binding {
                pat: SrcNode::new(Pat::Wildcard, span),
                binding: match ident.as_str() {
                    "_" => None,
                    _ => Some(SrcNode::new(ident, span)),
                }
            })
            .map_with_span(|pat, span| SrcNode::new(pat, span));

        litr_pat
            .or(nested_parser(binding.clone(), Delimiter::Paren)
                .map_with_span(|inner, span| SrcNode::new(Pat::Inner(inner), span)))
            .or(tuple_pat)
            .or(list_pat)
            .or(list_front_pat)
            .then(just(Token::Colon)
                .padding_for(ident_parser().map_with_span(|ident, span| SrcNode::new(ident, span)))
                .or_not())
            .map_with_span(|(pat, binding), span| SrcNode::new(Binding {
                pat,
                binding,
            }, span))
            .or(free_binding)
    })
}

fn expr_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<Expr>>, Error> {
    recursive(|expr| {
        let expr = expr.link();

        let ident = ident_parser().map(|ident| Expr::Path(Path(vec![ident])));
        let number = number_parser().map(|x| Expr::Literal(Literal::Number(x)));
        let boolean = just(Token::Boolean(true)).to(Literal::Boolean(true))
            .or(just(Token::Boolean(false)).to(Literal::Boolean(false)))
            .map(|b| Expr::Literal(b));
        let string = string_parser().map(|x| Expr::Literal(Literal::String(x)));
        let literal = ident
            .or(number)
            .or(boolean)
            .or(string)
            .map_with_span(|litr, span| SrcNode::new(litr, span));

        let brack_expr_list = nested_parser(
            expr.clone().separated_by(just(Token::Comma)),
            Delimiter::Brack,
        );

        let paren_expr_list = nested_parser(
            expr.clone().separated_by(just(Token::Comma)),
            Delimiter::Paren,
        );

        let binding = binding_parser()
            .then(just(Token::Of)
                .padding_for(type_parser())
                .or_not());

        let atom = literal
            // Parenthesised expression
            .or(nested_parser(expr.clone(), Delimiter::Paren))
            // Lists
            .or(brack_expr_list
                .map_with_span(|items, span| SrcNode::new(Expr::List(items), span)))
            // Tuples
            .or(paren_expr_list
                .clone()
                .map_with_span(|items, span| SrcNode::new(Expr::Tuple(items), span)))
            // Let
            .or(just(Token::Let)
                .padding_for(binding.clone())
                .padded_by(just(Token::Op(Op::Eq)))
                .then(expr.clone())
                .map_with_span(|(pat, expr), span| ((pat, expr), span))
                .padded_by(just(Token::In))
                .then(expr.clone())
                .map(|((((pat, pat_ty), expr), span), then)| SrcNode::new(Expr::Let(pat, pat_ty, expr, then), span)))
            // If
            .or(just(Token::If)
                .padding_for(expr.clone())
                .padded_by(just(Token::Then))
                .then(expr.clone())
                .padded_by(just(Token::Else))
                .then(expr.clone())
                .map_with_span(|((pred, a), b), span| SrcNode::new(Expr::If(pred, a, b), span)))
            .or(just(Token::Match)
                .padding_for(expr.clone())
                .padded_by(just(Token::In))
                .then(just(Token::Pipe)
                    .padding_for(binding.clone())
                    .padded_by(just(Token::RMap))
                    .then(expr.clone())
                    .repeated())
                .map_with_span(|(pred, arms), span| SrcNode::new(Expr::Match(pred, arms), span)))
            .boxed();

        let application = atom
            .then(paren_expr_list.repeated())
            .reduce_left(|f, args| args
                .into_iter()
                .fold(f, |f, arg| {
                    let span = f.span().union(arg.span());
                    SrcNode::new(Expr::Apply(f, arg), span)
                }))
            .boxed();

        let infix = application.clone()
            .then(just(Token::Colon).padding_for(application).repeated())
            .reduce_left(|arg, f| {
                let span = f.span().union(arg.span());
                SrcNode::new(Expr::Apply(f, arg), span)
            })
            .boxed();

        let unary = just(Token::Op(Op::Sub)).to(UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(UnaryOp::Not))
            .or(just(Token::Op(Op::Head)).to(UnaryOp::Head))
            .or(just(Token::Op(Op::Tail)).to(UnaryOp::Tail))
            .map_with_span(|op, span| SrcNode::new(op, span))
            .repeated()
            .then(infix)
            .reduce_right(|op, expr| {
                let span = op.span().union(expr.span());
                SrcNode::new(Expr::Unary(op, expr), span)
            })
            .boxed();

        let product_op = just(Token::Op(Op::Mul)).to(BinaryOp::Mul)
            .or(just(Token::Op(Op::Div)).to(BinaryOp::Div))
            .or(just(Token::Op(Op::Rem)).to(BinaryOp::Rem))
            .map_with_span(|op, span| SrcNode::new(op, span));
        let product = unary.clone()
            .then(product_op.then(unary).repeated())
            .reduce_left(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let sum_op = just(Token::Op(Op::Add)).to(BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(BinaryOp::Sub))
            .map_with_span(|op, span| SrcNode::new(op, span));
        let sum = product.clone()
            .then(sum_op.then(product).repeated())
            .reduce_left(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let join_op = just(Token::Op(Op::Join)).to(BinaryOp::Join)
            .map_with_span(|op, span| SrcNode::new(op, span));
        let join = sum.clone()
            .then(join_op.then(sum).repeated())
            .reduce_left(|a, (op, b)| {
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
            .map_with_span(|op, span| SrcNode::new(op, span));
        let comparison = join.clone()
            .then(comparison_op.then(join).repeated())
            .reduce_left(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        let logical_op = just(Token::Op(Op::And)).to(BinaryOp::And)
            .or(just(Token::Op(Op::Or)).to(BinaryOp::Or))
            .map_with_span(|op, span| SrcNode::new(op, span));
        let logical = comparison.clone()
            .then(logical_op.then(comparison).repeated())
            .reduce_left(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(Expr::Binary(op, a, b), span)
            })
            .boxed();

        /*
        // :( ambiguities
        let func = pat
            .padded_by(just(Token::RArrow))
            .then(expr)
            .map_with_span(|((param, param_ty), body), span| SrcNode::new(Expr::Func(param, param_ty, body), span));
        */

        let func = just(Token::Pipe)
            .padding_for(binding.separated_by(just(Token::Comma)))
            .padded_by(just(Token::Pipe))
            .then(expr)
            .reduce_right(|(param, param_ty), body| {
                let span = param
                    .span()
                    .union(param_ty
                        .as_ref()
                        .map(|t| t.span())
                        .unwrap_or(Span::none()))
                    .union(body.span());
                SrcNode::new(Expr::Func(param, param_ty, body), span)
            })
            .boxed();

        func
            .or(logical)
    })
}

pub fn parse_expr(tokens: &[node::Node<Token>]) -> Result<SrcNode<Expr>, Vec<Error>> {
    expr_parser()
        .padded_by(end())
        .parse(tokens.iter().cloned())
}

fn data_type_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<DataType>>, Error> {
    let variant = ident_parser()
        .map_with_span(|ident, span| SrcNode::new(ident, span))
        .then(type_parser()
            .or_not()
            .map(|ty| ty.unwrap_or_else(|| SrcNode::new(Type::Tuple(vec![]), Span::none()))));

    let sum = just(Token::Pipe)
        .or_not()
        .padding_for(variant.clone()
            .then(just(Token::Pipe).padding_for(variant).repeated())
            .map(|(head, tail)| (vec![head], tail))
            .reduce_left(|mut variants, variant| {
                variants.push(variant);
                variants
            }))
        .map_with_span(|variants, span| SrcNode::new(DataType::Sum(variants), span));

    let field = ident_parser()
        .map_with_span(|ident, span| SrcNode::new(ident, span))
        .then(type_parser());

    let product = just(Token::Dot)
        .padding_for(field)
        .repeated()
        .map_with_span(|fields, span| SrcNode::new(DataType::Product(fields), span));

    sum.or(product)
}

#[derive(Clone, Debug)]
pub enum DataType {
    Sum(Vec<(SrcNode<Ident>, SrcNode<Type>)>),
    Product(Vec<(SrcNode<Ident>, SrcNode<Type>)>),
}

#[derive(Debug)]
pub struct Def {
    pub generics: Vec<SrcNode<Ident>>,
    pub name: SrcNode<Ident>,
    pub ty: SrcNode<Type>,
    pub body: SrcNode<Expr>,
}

impl Def {
    pub fn main(body: SrcNode<Expr>) -> Self {
        Self {
            generics: Vec::new(),
            name: SrcNode::new(LocalIntern::new("main".to_string()), Span::none()),
            ty: SrcNode::new(Type::Unknown, Span::none()),
            body,
        }
    }
}

#[derive(Debug)]
pub struct TypeAlias {
    pub generics: Vec<SrcNode<Ident>>,
    pub name: SrcNode<Ident>,
    pub ty: SrcNode<Type>,
}

#[derive(Debug)]
pub struct Data {
    pub generics: Vec<SrcNode<Ident>>,
    pub name: SrcNode<Ident>,
    pub data_ty: SrcNode<DataType>,
}

#[derive(Debug)]
pub enum Decl {
    Def(Def),
    TypeAlias(TypeAlias),
    Data(Data),
}

#[derive(Default, Debug)]
pub struct Module {
    pub decls: Vec<SrcNode<Decl>>,
}

fn module_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<Module>>, Error> {
    recursive(|module| {
        let module = module.link();

        let generics = ident_parser()
            .map_with_span(|ident, span| SrcNode::new(ident, span))
            .separated_by(just(Token::Comma));

        // let typeparams = just(Token::Given)
        //     .padding_for(generics)
        //     .or_not()
        //     .map(|gs| gs.unwrap_or_default());

        let def = just(Token::Def)
            // Name
            .padding_for(ident_parser().map_with_span(|ident, span| SrcNode::new(ident, span)))
            // Generic parameters
            .then(generics.clone())
            // Optional type annotation
            .then(just(Token::Of)
                .padding_for(type_parser())
                .or_not())
            .padded_by(just(Token::Op(Op::Eq)))
            .then(expr_parser())
            .map_with_span(|(((name, generics), ty), body), span| Decl::Def(Def {
                generics,
                ty: ty.unwrap_or_else(|| SrcNode::new(Type::Unknown, name.span())),
                name,
                body,
            }));

        let type_alias = just(Token::Type)
            // Name
            .padding_for(ident_parser().map_with_span(|ident, span| SrcNode::new(ident, span)))
            // Generic parameters
            .then(generics.clone())
            .padded_by(just(Token::Op(Op::Eq)))
            .then(type_parser())
            .map_with_span(|((name, generics), ty), span| Decl::TypeAlias(TypeAlias {
                generics,
                name,
                ty,
            }));

        let data = just(Token::Data)
            // Name
            .padding_for(ident_parser().map_with_span(|ident, span| SrcNode::new(ident, span)))
            // Generic parameters
            .then(generics)
            .padded_by(just(Token::Op(Op::Eq)))
            .then(data_type_parser())
            .map_with_span(|((name, generics), data_ty), span| Decl::Data(Data {
                generics,
                name,
                data_ty,
            }));

        let decl = def
            .or(type_alias)
            .or(data)
            .map_with_span(|decl, span| SrcNode::new(decl, span));

        decl
            .repeated()
            .map_with_span(|decls, span| SrcNode::new(Module { decls }, span))
    })
}

pub fn parse_module(tokens: &[node::Node<Token>]) -> Result<SrcNode<Module>, Vec<Error>> {
    module_parser()
        .padded_by(end())
        .parse(tokens.iter().cloned())
}
