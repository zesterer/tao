use parze::prelude::*;
use internment::LocalIntern;
use crate::{
    lex::{Token, Delimiter, Op},
    src::SrcRegion,
    node::Node,
    error::Error,
};

#[derive(Debug)]
pub enum Literal {
    Null,
    Number(f64),
    String(String),
    Boolean(bool),
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,

    Not,

    Head,
    Tail,
}

impl UnaryOp {
    pub fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region)
    }
}

#[derive(Copy, Clone, Debug)]
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
}

impl BinaryOp {
    pub fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region)
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Ident(Node<LocalIntern<String>>),
    Unary(Node<UnaryOp>, Node<Expr>),
    Binary(Node<BinaryOp>, Node<Expr>, Node<Expr>),
    Branch(Node<Expr>, Node<Expr>, Node<Expr>),
    Func(Node<LocalIntern<String>>, Node<Expr>),
    Apply(Node<Expr>, Node<Expr>),
    List(Vec<Node<Expr>>),
}

impl Expr {
    pub fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region)
    }
}

pub fn parse(tokens: &[Node<Token>]) -> Result<Node<Expr>, Vec<Error>> {
    let expr = recursive(|expr| {
        let expr = expr.link();

        let literal = permit_map(|token: Node<Token>| Some(match &*token {
            Token::Null => Expr::Literal(Literal::Null),
            Token::Number(x) => Expr::Literal(Literal::Number(x.parse().unwrap())),
            Token::String(x) => Expr::Literal(Literal::String(x.to_string())),
            Token::Boolean(x) => Expr::Literal(Literal::Boolean(*x)),
            _ => return None,
        }.at(token.region())));

        let ident = permit_map(|token: Node<Token>| match &*token {
            Token::Ident(x) => Some(Node::new(*x, token.region())),
            _ => None,
        });

        let paren_expr = nested_parse({
            let expr = expr.clone();
            move |token: Node<Token>| match token.into_inner() {
                Token::Tree(Delimiter::Paren, tokens) => Some((expr.clone(), tokens)),
                _ => None,
            }
        }).boxed();

        let paren_expr_list = nested_parse({
            let expr = expr.clone();
            move |token: Node<Token>| match token.into_inner() {
                Token::Tree(Delimiter::Paren, tokens) => Some((expr.clone().separated_by(just(Token::Comma)), tokens)),
                _ => None,
            }
        }).boxed();

        let brack_expr_list = nested_parse({
            let expr = expr.clone();
            move |token: Node<Token>| match token.into_inner() {
                Token::Tree(Delimiter::Brack, tokens) => Some((expr.clone().separated_by(just(Token::Comma)), tokens)),
                _ => None,
            }
        }).boxed();

        let atom = literal
            .or(paren_expr.clone())
            .or(brack_expr_list.map(|items| Expr::List(items).at(SrcRegion::none()))) // TODO!
            .or(just(Token::If)
                .padding_for(expr.clone())
                .padded_by(just(Token::Then))
                .then(expr.clone())
                .padded_by(just(Token::Else))
                .then(expr.clone())
                .map(|((p, t), f)| Expr::Branch(p, t, f)
                    .at(SrcRegion::none())))
            .or(just(Token::Let)
                .padding_for(ident.clone())
                .padded_by(just(Token::Op(Op::Eq)))
                .then(expr.clone())
                .padded_by(just(Token::Ident(LocalIntern::new("in".to_string()))))
                .then(expr.clone())
                .map(|((name, val), then)| Expr::Apply(Expr::Func(name, then).at(SrcRegion::none()), val)
                    .at(SrcRegion::none())))
            .or(ident.clone().map(|x| Expr::Ident(x).at(SrcRegion::none())))
            .boxed();

        let application = atom.clone()
            .then(paren_expr_list.repeated())
            .reduce_left(|f, args| args
                .into_iter()
                .fold(f, |f, arg| {
                    let region = f.region().union(arg.region());
                    Expr::Apply(f, arg).at(region)
                }));

        let infix = application.clone()
            .then(just(Token::Colon).padding_for(application).repeated())
            .reduce_left(|arg, f| {
                let region = f.region().union(arg.region());
                Expr::Apply(f, arg)
                    .at(region)
            });

        let unary_op = just(Token::Op(Op::Sub)).map(|t: Node<Token>| t.map_inner(|_| UnaryOp::Neg))
            .or(just(Token::Op(Op::Not)).map(|t: Node<Token>| t.map_inner(|_| UnaryOp::Not)))
            .or(just(Token::Op(Op::Head)).map(|t: Node<Token>| t.map_inner(|_| UnaryOp::Head)))
            .or(just(Token::Op(Op::Tail)).map(|t: Node<Token>| t.map_inner(|_| UnaryOp::Tail)));
        let unary = unary_op.repeated().then(infix)
            .reduce_right(|op, expr| {
                let region = op.region().union(expr.region());
                Expr::Unary(op, expr)
                    .at(region)
            });

        let product_op = just(Token::Op(Op::Mul)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Mul))
            .or(just(Token::Op(Op::Div)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Div)))
            .or(just(Token::Op(Op::Rem)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Rem)));
        let product = unary.clone().then(product_op.then(unary).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                Expr::Binary(op, a, b)
                    .at(region)
            });

        let sum_op = just(Token::Op(Op::Add)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Add))
            .or(just(Token::Op(Op::Sub)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Sub)));
        let sum = product.clone().then(sum_op.then(product).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                Expr::Binary(op, a, b)
                    .at(region)
            });

        let join_op = just(Token::Op(Op::Join)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Join));
        let join = sum.clone().then(join_op.then(sum).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                Expr::Binary(op, a, b)
                    .at(region)
            });

        let comparison_op = just(Token::Op(Op::Eq)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Eq))
            .or(just(Token::Op(Op::Less)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::Less)))
            .or(just(Token::Op(Op::More)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::More)))
            .or(just(Token::Op(Op::LessEq)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::LessEq)))
            .or(just(Token::Op(Op::MoreEq)).map(|t: Node<Token>| t.map_inner(|_| BinaryOp::MoreEq)));
        let comparison = join.clone().then(comparison_op.then(join).repeated())
            .reduce_left(|a, (op, b)| {
                let region = a.region().union(b.region());
                Expr::Binary(op, a, b)
                    .at(region)
            });

        let func = ident
            .padded_by(just(Token::RArrow))
            .then(expr)
            .map(|(name, body)| Expr::Func(name, body)
                .at(SrcRegion::none()));

        func.or(comparison)
    });

    expr
        .padded_by(end())
        .parse(tokens.iter().cloned())
}