use parze::prelude::*;
use internment::Intern;
use crate::lex::Token;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Value),
    Ident(Intern<String>),
}

pub fn parse(tokens: &[Token]) -> Result<Expr, DefaultError<Token>> {
    let literal = permit_map(|tok| match tok {
        Token::Number(x) => Some(Expr::Literal(Value::Number(x))),
        Token::String(x) => Some(Expr::Literal(Value::String((*x).clone()))),
        Token::Bool(x) => Some(Expr::Literal(Value::Bool(x))),
        _ => None,
    });

    let atom = literal;

    let expr = atom;

    expr
        .parse(tokens.iter().copied())
        .map_err(|mut errs| errs.remove(0))
}
