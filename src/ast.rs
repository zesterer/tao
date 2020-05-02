use std::fmt;
use internment::LocalIntern;
use parze::prelude::*;
use crate::{
    node,
    lex::{Token, Delimiter, Op},
    error::Error,
    node2::SrcNode,
    src::SrcRegion,
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eq,
    Less,
    More,
    LessEq,
    MoreEq,

    Join,

    Apply,
}

#[derive(Debug)]
pub enum Pat {
    Wildcard,
    Literal(Literal),
    Ident(Ident),
    List(Vec<SrcNode<Pat>>),
    ListFront(Vec<SrcNode<Pat>>),
    ListBack(Vec<SrcNode<Pat>>),
    Tuple(Vec<SrcNode<Pat>>),
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
    Match(SrcNode<Self>, Vec<(SrcNode<Pat>, SrcNode<Self>)>),
    Func(SrcNode<Pat>, Option<SrcNode<Type>>, SrcNode<Self>),
    Apply(SrcNode<Self>, SrcNode<Self>),
    Let(SrcNode<Pat>, Option<SrcNode<Type>>, SrcNode<Self>, SrcNode<Self>),
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
                    .map_with_region(|ident, region| SrcNode::new(Type::Data(SrcNode::new(ident, region), Vec::new()), region));

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
                        .map_with_region(|ty, region| SrcNode::new(ty, region)));

                atom
            })
        };

        let data = ident_parser()
            .map_with_region(|name, region| SrcNode::new(name, region))
            .then(atom.clone().repeated())
            .map(|(data, params)| Type::Data(data, params))
            .map_with_region(|ty, region| SrcNode::new(ty, region))
            .or(atom);

        data.clone()
            .then(just(Token::RArrow).padding_for(ty.clone()).repeated())
            .reduce_left(|i, o| {
                let region = i.region().union(o.region());
                SrcNode::new(Type::Func(i, o), region)
            })
            .or(data)
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
            .map_with_region(|litr, region| SrcNode::new(litr, region));

        let brack_expr_list = nested_parser(
            expr
                .clone()
                .separated_by(just(Token::Comma)),
            Delimiter::Brack,
        );

        let paren_expr_list = nested_parser(
            expr
                .clone()
                .separated_by(just(Token::Comma)),
            Delimiter::Paren,
        );

        let pat = ident_parser()
            .map_with_region(|ident, region| SrcNode::new(Pat::Ident(ident), region))
            .then(just(Token::Of)
                .padding_for(type_parser())
                .or_not());

        let atom = literal
            // Parenthesised expression
            .or(nested_parser(expr.clone(), Delimiter::Paren))
            // Lists
            .or(brack_expr_list
                .map_with_region(|items, region| SrcNode::new(Expr::List(items), region)))
            // Tuples
            .or(paren_expr_list
                .clone()
                .map_with_region(|items, region| SrcNode::new(Expr::Tuple(items), region)))
            // Let
            .or(just(Token::Let)
                .padding_for(pat.clone())
                .padded_by(just(Token::Op(Op::Eq)))
                .then(expr.clone())
                .map_with_region(|(pat, expr), region| ((pat, expr), region))
                .padded_by(just(Token::In))
                .then(expr.clone())
                .map(|((((pat, pat_ty), expr), region), then)| SrcNode::new(Expr::Let(pat, pat_ty, expr, then), region)))
            // If
            .or(just(Token::If)
                .padding_for(expr.clone())
                .padded_by(just(Token::Then))
                .then(expr.clone())
                .padded_by(just(Token::Else))
                .then(expr.clone())
                .map_with_region(|((pred, a), b), region| SrcNode::new(Expr::If(pred, a, b), region)))
            .boxed();

        let application = atom
            .then(paren_expr_list.repeated())
            .reduce_left(|f, args| args
                .into_iter()
                .fold(f, |f, arg| {
                    let region = f.region().union(arg.region());
                    SrcNode::new(Expr::Apply(f, arg), region)
                }));

        let infix = application.clone()
            .then(just(Token::Colon).padding_for(application).repeated())
            .reduce_left(|arg, f| {
                let region = f.region().union(arg.region());
                SrcNode::new(Expr::Apply(f, arg), region)
            });

        let unary = just(Token::Op(Op::Sub)).to(UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(UnaryOp::Not))
            .or(just(Token::Op(Op::Head)).to(UnaryOp::Head))
            .or(just(Token::Op(Op::Tail)).to(UnaryOp::Tail))
            .map_with_region(|op, region| SrcNode::new(op, region))
            .repeated()
            .then(infix)
            .reduce_right(|op, expr| {
                let region = op.region().union(expr.region());
                SrcNode::new(Expr::Unary(op, expr), region)
            });

        let product_op = just(Token::Op(Op::Mul)).to(BinaryOp::Mul)
            .or(just(Token::Op(Op::Div)).to(BinaryOp::Div))
            .or(just(Token::Op(Op::Rem)).to(BinaryOp::Rem))
            .map_with_region(|op, region| SrcNode::new(op, region));
        let product = unary.clone()
            .then(product_op.then(unary).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                SrcNode::new(Expr::Binary(op, a, b), region)
            });

        let sum_op = just(Token::Op(Op::Add)).to(BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(BinaryOp::Sub))
            .map_with_region(|op, region| SrcNode::new(op, region));
        let sum = product.clone()
            .then(sum_op.then(product).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                SrcNode::new(Expr::Binary(op, a, b), region)
            });

        let join_op = just(Token::Op(Op::Join)).to(BinaryOp::Join)
            .map_with_region(|op, region| SrcNode::new(op, region));
        let join = sum.clone()
            .then(join_op.then(sum).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                SrcNode::new(Expr::Binary(op, a, b), region)
            });

        let comparison_op = just(Token::Op(Op::Eq)).to(BinaryOp::Eq)
            .or(just(Token::Op(Op::Less)).to(BinaryOp::Less))
            .or(just(Token::Op(Op::More)).to(BinaryOp::More))
            .or(just(Token::Op(Op::LessEq)).to(BinaryOp::LessEq))
            .or(just(Token::Op(Op::MoreEq)).to(BinaryOp::MoreEq))
            .map_with_region(|op, region| SrcNode::new(op, region));
        let comparison = join.clone()
            .then(comparison_op.then(join).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                SrcNode::new(Expr::Binary(op, a, b), region)
            });

        /*
        // :( ambiguities
        let func = pat
            .padded_by(just(Token::RArrow))
            .then(expr)
            .map_with_region(|((param, param_ty), body), region| SrcNode::new(Expr::Func(param, param_ty, body), region));
        */

        let func = just(Token::Pipe)
            .padding_for(pat.separated_by(just(Token::Comma)))
            .padded_by(just(Token::Pipe))
            .then(expr)
            .reduce_right(|(param, param_ty), body| {
                let region = param
                    .region()
                    .union(param_ty
                        .as_ref()
                        .map(|t| t.region())
                        .unwrap_or(SrcRegion::none()))
                    .union(body.region());
                SrcNode::new(Expr::Func(param, param_ty, body), region)
            });

        func
            .or(comparison)
    })
}

pub fn parse_expr(tokens: &[node::Node<Token>]) -> Result<SrcNode<Expr>, Vec<Error>> {
    expr_parser()
        .padded_by(end())
        .parse(tokens.iter().cloned())
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
            name: SrcNode::new(LocalIntern::new("main".to_string()), SrcRegion::none()),
            ty: SrcNode::new(Type::Unknown, SrcRegion::none()),
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
pub enum Decl {
    Def(Def),
    TypeAlias(TypeAlias),
}

#[derive(Default, Debug)]
pub struct Module {
    pub decls: Vec<SrcNode<Decl>>,
}

fn module_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<Module>>, Error> {
    recursive(|module| {
        let module = module.link();

        let generics = ident_parser()
            .map_with_region(|ident, region| SrcNode::new(ident, region))
            .separated_by(just(Token::Comma));

        let given_params = just(Token::Given)
            .padding_for(generics)
            .or_not()
            .map(|gs| gs.unwrap_or_default());

        let def = given_params.clone()
            .padded_by(just(Token::Def))
            // Name
            .then(ident_parser().map_with_region(|ident, region| SrcNode::new(ident, region)))
            // Optional type annotation
            .then(just(Token::Of)
                .padding_for(type_parser())
                .or_not()
                .map(|ty_hint| ty_hint.unwrap_or_else(|| SrcNode::new(Type::Unknown, SrcRegion::none()))))
            .padded_by(just(Token::Op(Op::Eq)))
            .then(expr_parser())
            .map_with_region(|(((generics, name), ty), body), region| Decl::Def(Def {
                generics,
                name,
                ty,
                body,
            }));

        let type_alias = given_params
            .padded_by(just(Token::Type))
            // Name
            .then(ident_parser().map_with_region(|ident, region| SrcNode::new(ident, region)))
            .padded_by(just(Token::Op(Op::Eq)))
            .then(type_parser())
            .map_with_region(|((generics, name), ty), region| Decl::TypeAlias(TypeAlias {
                generics,
                name,
                ty,
            }));

        let decl = def
            .or(type_alias)
            .map_with_region(|decl, region| SrcNode::new(decl, region));

        decl
            .repeated()
            .map_with_region(|decls, region| SrcNode::new(Module { decls }, region))
    })
}

pub fn parse_module(tokens: &[node::Node<Token>]) -> Result<SrcNode<Module>, Vec<Error>> {
    module_parser()
        .padded_by(end())
        .parse(tokens.iter().cloned())
}
