use internment::LocalIntern;
use parze::prelude::*;
use crate::{
    node,
    lex::{Token, Delimiter, Op},
    error::Error,
    node2::SrcNode,
};

type Ident = LocalIntern<String>;

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    String(LocalIntern<String>),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Path(Vec<Ident>);

impl Path {
    pub fn parts(&self) -> &[Ident] {
        &self.0
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

#[derive(Debug)]
pub enum Type {
    Ident(Ident),
    Complex(SrcNode<Type>, Vec<SrcNode<Type>>),
    Tuple(Vec<SrcNode<Type>>),
    Func(SrcNode<Type>, SrcNode<Type>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Path(Path),
    Unary(SrcNode<UnaryOp>, SrcNode<Expr>),
    Binary(SrcNode<BinaryOp>, SrcNode<Expr>, SrcNode<Expr>),
    If(SrcNode<Expr>, SrcNode<Expr>, SrcNode<Expr>),
    Match(SrcNode<Expr>, Vec<(SrcNode<Pat>, SrcNode<Expr>)>),
    Func(SrcNode<Pat>, SrcNode<Expr>),
    Apply(SrcNode<Expr>, SrcNode<Expr>),
    Let(SrcNode<Pat>, Option<SrcNode<Type>>, SrcNode<Expr>, SrcNode<Expr>),
    List(Vec<SrcNode<Expr>>),
    Tuple(Vec<SrcNode<Expr>>),
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

fn expr_parser() -> Parser<impl Pattern<Error, Input=node::Node<Token>, Output=SrcNode<Expr>>, Error> {
    recursive(|expr| {
        let expr = expr.link();

        let ident = ident_parser().map(|ident| Expr::Path(Path(vec![ident])));
        let number = number_parser().map(|x| Expr::Literal(Literal::Number(x)));
        let boolean = just(Token::Boolean(true)).to(Literal::Boolean(true))
            .or(just(Token::Boolean(false)).to(Literal::Boolean(false)))
            .map(|b| Expr::Literal(b));
        let literal = ident
            .or(number)
            .or(boolean)
            .map_with_region(|litr, region| SrcNode::new(litr, region));

        let list = nested_parser(
            expr
                .clone()
                .separated_by(just(Token::Comma)),
            Delimiter::Brack,
        )
            .map_with_region(|items, region| SrcNode::new(Expr::List(items), region));

        let tuple = nested_parser(
            expr
                .clone()
                .separated_by(just(Token::Comma)),
            Delimiter::Paren,
        )
            .map_with_region(|items, region| SrcNode::new(Expr::Tuple(items), region));

        let pat = ident_parser()
            .map_with_region(|ident, region| SrcNode::new(Pat::Ident(ident), region));

        let func = pat.clone()
            .padded_by(just(Token::RArrow))
            .then(expr.clone())
            .map_with_region(|(param, body), region| SrcNode::new(Expr::Func(param, body), region));

        let atom = func
            .or(literal)
            .or(nested_parser(expr.clone(), Delimiter::Paren))
            .or(list)
            .or(tuple);

        let unary = just(Token::Op(Op::Sub)).to(UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(UnaryOp::Not))
            .or(just(Token::Op(Op::Head)).to(UnaryOp::Head))
            .or(just(Token::Op(Op::Tail)).to(UnaryOp::Tail))
            .map_with_region(|op, region| SrcNode::new(op, region))
            .repeated()
            .then(atom)
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

        sum
    })
}

pub fn parse_expr(tokens: &[node::Node<Token>]) -> Result<SrcNode<Expr>, Vec<Error>> {
    expr_parser()
        .padded_by(end())
        .parse(tokens.iter().cloned())
}
