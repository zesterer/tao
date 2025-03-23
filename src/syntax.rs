use super::*;
use chumsky::{
    input::{BorrowInput, StrInput},
    pratt::*,
    prelude::*,
};
use std::{fmt, path::PathBuf};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Filename(pub ArcIntern<PathBuf>);

impl fmt::Display for Filename {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

pub type Span = SimpleSpan<usize, Filename>;

pub type Ident = ArcIntern<String>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Ident(Ident),
    Nat(u64),
    // Keywords
    Def,
    Let,
    In,
    // Operators
    Eq,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenTree {
    Token(Token),
    Tree(Vec<SrcNode<TokenTree>>),
}

pub fn lexer<'src, I>(
) -> impl Parser<'src, I, Vec<SrcNode<TokenTree>>, extra::Err<Rich<'src, char, Span>>>
where
    I: StrInput<'src, Token = char, Span = Span, Slice = &'src str>,
{
    recursive(|tt| {
        let token = choice((
            // Keywords
            text::keyword("def").to(Token::Def),
            text::keyword("let").to(Token::Let),
            text::keyword("in").to(Token::In),
            // Punctuation
            just("=").to(Token::Eq),
            // Identifiers
            text::ident().map(ArcIntern::from_ref).map(Token::Ident),
            // Numbers
            text::int(10).map(|s: &str| Token::Nat(s.parse().unwrap())),
        ));

        choice((token.map(TokenTree::Token),))
            .map_with(|tt, e| SrcNode::new(tt, e.span()))
            .padded()
    })
    .repeated()
    .collect()
    .padded()
}

#[derive(Debug)]
pub enum Expr {
    Nat(u64),
    Local(Ident),
    Let(SrcNode<Ident>, Box<SrcNode<Expr>>, Box<SrcNode<Expr>>),
}

#[derive(Debug)]
pub struct Def {
    pub name: SrcNode<Ident>,
    pub body: SrcNode<Expr>,
}

#[derive(Debug)]
pub enum Item {
    Def(Def),
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<SrcNode<Item>>,
}

pub struct Parsers<E, M> {
    pub expr: E,
    pub module: M,
}

pub fn parsers<'src, I>() -> Parsers<
    impl Parser<'src, I, SrcNode<Expr>, extra::Err<Rich<'src, TokenTree, Span>>>,
    impl Parser<'src, I, SrcNode<Module>, extra::Err<Rich<'src, TokenTree, Span>>>,
>
where
    I: BorrowInput<'src, Token = TokenTree, Span = Span>,
{
    let mut parsers = Parsers {
        expr: Recursive::declare(),
        module: Recursive::declare(),
    };

    let tok = |tok| just(TokenTree::Token(tok));
    let ident = select_ref! { TokenTree::Token(Token::Ident(ident)) = e => SrcNode::new(ident.clone(), e.span()) };

    {
        let atom = select_ref! {
            // Literals
            TokenTree::Token(Token::Nat(x)) => Expr::Nat(*x),
            // Identifiers
            TokenTree::Token(Token::Ident(x)) => Expr::Local(x.clone()),
        }
        .map_with(|expr, e| SrcNode::new(expr, e.span()));

        parsers.expr.define(
            atom.pratt((
                // let x = y in z
                prefix(
                    1,
                    tok(Token::Let)
                        .ignore_then(ident)
                        .then_ignore(tok(Token::Eq))
                        .then(parsers.expr.clone())
                        .then_ignore(tok(Token::In)),
                    |(local, rhs), then, e| {
                        SrcNode::new(Expr::Let(local, Box::new(rhs), Box::new(then)), e.span())
                    },
                ),
            )),
        );
    }

    {
        let def = tok(Token::Def)
            .ignore_then(ident)
            .then_ignore(tok(Token::Eq))
            .then(parsers.expr.clone())
            .map(|(name, body)| Def { name, body });

        let item = choice((def.map(Item::Def),)).map_with(|tt, e| SrcNode::new(tt, e.span()));

        parsers.module.define(
            item.repeated()
                .collect()
                .map_with(|items, e| SrcNode::new(Module { items }, e.span())),
        );
    }

    parsers
}
