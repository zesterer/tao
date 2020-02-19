use std::fmt;
use parze::prelude::*;
use internment::LocalIntern;
use crate::{
    lex::{Token, Delimiter, Op},
    src::SrcRegion,
    node::Node,
    error::Error,
    hir::TypeInfo,
};

#[derive(Debug)]
pub enum Literal {
    Null,
    Number(f64),
    String(String),
    Boolean(bool),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Neg,

    Not,

    Head,
    Tail,
}

impl UnaryOp {
    pub fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region, ())
    }
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
    Less,
    More,
    LessEq,
    MoreEq,

    Join,
}

impl BinaryOp {
    pub fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region, ())
    }
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
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::More => write!(f, ">"),
            BinaryOp::LessEq => write!(f, "<="),
            BinaryOp::MoreEq => write!(f, ">="),
            BinaryOp::Join => write!(f, "++"),
        }
    }
}

type NodeExpr = Node<Expr, Node<TypeInfo>>;

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Ident(Node<LocalIntern<String>>),
    Unary(Node<UnaryOp>, NodeExpr),
    Binary(Node<BinaryOp>, NodeExpr, NodeExpr),
    Branch(NodeExpr, NodeExpr, NodeExpr),
    Func(Node<LocalIntern<String>, Node<TypeInfo>>, NodeExpr),
    Apply(NodeExpr, NodeExpr),
    List(Vec<NodeExpr>),
    Tuple(Vec<NodeExpr>),
}

#[derive(Clone, Debug)]
pub struct TypeParam(Node<LocalIntern<String>>);

impl TypeParam {
    pub fn name(&self) -> &Node<LocalIntern<String>> {
        &self.0
    }
}

impl Expr {
    pub fn at(self, region: SrcRegion) -> NodeExpr {
        Node::new(self, region, Node::new(TypeInfo::default(), region, ()))
    }
}

#[derive(Debug)]
pub enum Decl {
    Value(Node<LocalIntern<String>>, Vec<TypeParam>, NodeExpr),
}

#[derive(Debug)]
pub struct Module {
    pub decls: Vec<Node<Decl>>,
}

fn ident_parser() -> Parser<impl Pattern<Error, Input=Node<Token>, Output=Node<LocalIntern<String>, Node<TypeInfo>>>, Error> {
    permit_map(|token: Node<Token>| match &*token {
        Token::Ident(x) => Some(Node::new(*x, token.region(), Node::new(TypeInfo::default(), token.region(), ()))),
        _ => None,
    })
}

fn type_param_parser() -> Parser<impl Pattern<Error, Input=Node<Token>, Output=TypeParam>, Error> {
    ident_parser()
        .map(|ident| TypeParam(Node::new(*ident.inner, ident.region, ())))
}

fn type_parser() -> Parser<impl Pattern<Error, Input=Node<Token>, Output=Node<TypeInfo>>, Error> {
    recursive(|ty| {
        let ty = ty.link();

        let atom_ty = {
            let ty = ty.clone();
            recursive(move |atom_ty| {
                let atom_ty = atom_ty.link();

                let paren_ty = nested_parse({
                    let ty = ty.clone();
                    move |token: Node<Token>| {
                        let region = token.region();
                        match token.into_inner() {
                            Token::Tree(Delimiter::Paren, tokens) =>
                                Some((ty
                                    .clone()
                                    .padded_by(end())
                                    .map_err(move |e: Error| e.at(region))
                                    .map(move |ty: Node<TypeInfo>| ty.at(region)), tokens)),
                            _ => None,
                        }
                    }
                });

                let paren_ty_list = nested_parse({
                    let ty = ty.clone();
                    move |token: Node<Token>| {
                        let region = token.region();
                        match token.into_inner() {
                            Token::Tree(Delimiter::Paren, tokens) =>
                                Some((ty
                                    .clone()
                                    .separated_by(just(Token::Comma))
                                    .padded_by(end())
                                    .map_err(move |e: Error| e.at(region)), tokens)),
                            _ => None,
                        }
                    }
                });

                let ident = permit_map(|token: Node<Token>| match &*token {
                    Token::Ident(x) => Some(Node::new(TypeInfo::from(Node::new(*x, token.region, ())), token.region(), ())),
                    _ => None,
                });

                let unknown = just(Token::QuestionMark)
                    .map_with_region(|_, region| Node::new(TypeInfo::Unknown, region, ()));

                let list = just(Token::Ident(LocalIntern::new("List".to_string())))
                    .padding_for(atom_ty.clone())
                    .map_with_region(|inner, region| Node::new(TypeInfo::List(inner), region, ()));

                let tuple = paren_ty_list
                    .map_with_region(|items, region| Node::new(TypeInfo::Tuple(items), region, ()));

                list
                    .or(paren_ty)
                    .or(tuple)
                    .or(unknown)
                    .or(ident)
            })
        };

        atom_ty
            .then(just(Token::RArrow).padding_for(ty.clone()).repeated())
            .reduce_left(|i, o| {
                let region = i.region().union(o.region());
                Node::new(TypeInfo::Func(i, o), region, ())
            })
    })
}

fn expr_parser() -> Parser<impl Pattern<Error, Input=Node<Token>, Output=NodeExpr>, Error> {
    recursive(|expr| {
        let expr = expr.link();

        let literal = permit_map(|token: Node<Token>| Some(match &*token {
            Token::Null => Expr::Literal(Literal::Null),
            Token::Number(x) => Expr::Literal(Literal::Number(x.parse().unwrap())),
            Token::String(x) => Expr::Literal(Literal::String(x.to_string())),
            Token::Boolean(x) => Expr::Literal(Literal::Boolean(*x)),
            _ => return None,
        }));

        let paren_expr = nested_parse({
            let expr = expr.clone();
            move |token: Node<Token>| {
                match token.into_inner() {
                    Token::Tree(Delimiter::Paren, tokens) =>
                        Some((expr
                            .clone()
                            .padded_by(end()), tokens)),
                    _ => None,
                }
            }
        }).boxed();

        let paren_expr_list = nested_parse({
            let expr = expr.clone();
            move |token: Node<Token>| {
                match token.into_inner() {
                    Token::Tree(Delimiter::Paren, tokens) => Some((expr
                        .clone()
                        .separated_by(just(Token::Comma))
                        .padded_by(end()), tokens)),
                    _ => None,
                }
            }
        }).boxed();

        let brack_expr_list = nested_parse({
            let expr = expr.clone();
            move |token: Node<Token>| {
                match token.into_inner() {
                    Token::Tree(Delimiter::Brack, tokens) =>
                        Some((expr
                            .clone()
                            .separated_by(just(Token::Comma))
                            .padded_by(end()), tokens)),
                    _ => None,
                }
            }
        }).boxed();

        let atom = paren_expr.clone()
            .or(literal
            .or(brack_expr_list.map(|items| Expr::List(items)))
            .or(paren_expr_list.clone().map(|items| Expr::Tuple(items)))
            .or(just(Token::If)
                .padding_for(expr.clone())
                .padded_by(just(Token::Then))
                .then(expr.clone())
                .padded_by(just(Token::Else))
                .then(expr.clone())
                .map(|((p, t), f)| Expr::Branch(p, t, f)))
            .or(just(Token::Let)
                .padding_for(ident_parser())
                .then(just(Token::Of).padding_for(type_parser()).or_not())
                .padded_by(just(Token::Op(Op::Eq)))
                .then(expr.clone())
                .padded_by(just(Token::In))
                .then(expr.clone())
                .map(|(((mut name, ty), val), then)| {
                    let f_region = name.region.union(then.region);
                    let region = f_region.union(val.region);
                    if let Some(ty) = ty {
                        name.meta = ty;
                    }
                    Expr::Apply(Expr::Func(name, then).at(f_region), val)
                }))
            .or(ident_parser().map(|x| Expr::Ident(Node::new(*x.inner(), x.region(), ()))))
            .map_with_region(|expr, region| expr.at(region)))
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

        let func = ident_parser()
            .padded_by(just(Token::RArrow))
            .then(expr)
            .map(|(name, body)| {
                let region = name.region.union(body.region);
                Expr::Func(name, body)
                    .at(region)
            });

        func.or(comparison)
    })
}

pub fn parse_expr(tokens: &[Node<Token>]) -> Result<NodeExpr, Vec<Error>> {
    expr_parser()
        .padded_by(end())
        .parse(tokens.iter().cloned())
}

pub fn parse_module(tokens: &[Node<Token>]) -> Result<Module, Vec<Error>> {
    let def = just(Token::Def)
        .padding_for(ident_parser())
        .then(type_param_parser().repeated())
        .then(just(Token::Of).padding_for(type_parser()).or_not())
        .padded_by(just(Token::Op(Op::Eq)))
        .then(expr_parser())
        .map_with_region(|(((name, ty_params), ty), mut body), region| {
            if let Some(ty) = ty {
                body.meta = ty;
            }
            Node::new(Decl::Value(Node::new(*name.inner, name.region, ()), ty_params, body), region, ())
        });

    let decl = def;

    decl
        .repeated()
        .map(|decls| Module {
            decls,
        })
        .padded_by(end())
        .parse(tokens.iter().cloned())
}
