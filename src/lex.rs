use std::{
    fmt,
    cmp::PartialEq,
};
use parze::prelude::*;
use internment::LocalIntern;
use crate::{
    src::SrcRegion,
    node::Node,
    error::Error,
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Op {
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

    Not,
    Head,
    Tail,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Rem => write!(f, "%"),
            Op::Eq => write!(f, "="),
            Op::Less => write!(f, "<"),
            Op::More => write!(f, ">"),
            Op::LessEq => write!(f, "<="),
            Op::MoreEq => write!(f, ">="),
            Op::Join => write!(f, "++"),
            Op::Not => write!(f, "!"),
            Op::Head => write!(f, "<:"),
            Op::Tail => write!(f, ":>"),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Delimiter {
    Paren,
    Brack,
    Brace,
}

impl Delimiter {
    fn left(&self) -> char {
        match self {
            Delimiter::Paren => '(',
            Delimiter::Brack => '[',
            Delimiter::Brace => '{',
        }
    }

    fn right(&self) -> char {
        match self {
            Delimiter::Paren => ')',
            Delimiter::Brack => ']',
            Delimiter::Brace => '}',
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Token {
    Number(LocalIntern<String>),
    Boolean(bool),
    String(LocalIntern<String>),
    Null,
    Ident(LocalIntern<String>),
    Op(Op),
    Tree(Delimiter, Vec<Node<Token>>),

    RArrow,
    Comma,
    Colon,

    Let,
    If,
    Then,
    Else,
    Def,
    In,
    Of,
}

impl Token {
    fn at(self, region: SrcRegion) -> Node<Self> {
        Node::new(self, region, ())
    }
}

impl PartialEq<Node<Token>> for Token {
    fn eq(&self, other: &Node<Token>) -> bool {
        self == &**other
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Number(x) => write!(f, "{}", x),
            Token::Boolean(x) => write!(f, "{}", x),
            Token::String(x) => write!(f, "\"{}\"", x),
            Token::Null => write!(f, "null"),
            Token::Ident(i) => write!(f, "{}", i),
            Token::Op(op) => write!(f, "{}", op),
            Token::Tree(delim, tokens) => write!(f, "{}...{}", delim.left(), delim.right()),
            Token::RArrow => write!(f, "->"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Def => write!(f, "def"),
            Token::In => write!(f, "in"),
            Token::Of => write!(f, "of"),
        }
    }
}

pub fn lex(code: &str) -> Result<Vec<Node<Token>>, Vec<Error>> {
    let tokens = recursive(|tokens| {
        let whitespace = permit(|c: &char| c.is_whitespace()).to(())
            .or(just('#').padding_for(permit(|c: &char| *c != '\n').repeated()).to(()));

        let space = whitespace.repeated();

        let integer = permit(|c: &char| c.is_ascii_digit()).once_or_more();

        let number = integer.clone()
            .then(just('.').padding_for(integer).or_not())
            .map_with_range(|(mut int, fract), range| {
                if let Some(mut fract) = fract {
                    int.push('.');
                    int.append(&mut fract);
                }
                Token::Number(LocalIntern::new(int.into_iter().collect()))
                    .at(range.into())
            });

        let special = just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'));
        let escape = just('\\').padding_for(special);
        let string = just('"')
            .padding_for(permit(|c: &char| *c != '\\' && *c != '"').or(escape).repeated())
            .padded_by(just('"'))
            .map_with_range(|chars, range| {
                Token::String(LocalIntern::new(chars.into_iter().collect()))
                    .at(range.into())
            })
            .boxed();

        let ident = permit(|c: &char| c.is_ascii_alphabetic() || *c == '_').once_or_more()
            .map_with_range(|chars, range| {
                Token::Ident(LocalIntern::new(chars.into_iter().collect()))
                    .at(range.into())
            });

        let op = seq("->".chars()).to(Token::RArrow)
            .or(seq("++".chars()).to(Token::Op(Op::Join)))
            .or(just('+').to(Token::Op(Op::Add)))
            .or(just('-').to(Token::Op(Op::Sub)))
            .or(just('*').to(Token::Op(Op::Mul)))
            .or(just('/').to(Token::Op(Op::Div)))
            .or(just('%').to(Token::Op(Op::Rem)))
            .or(just('=').to(Token::Op(Op::Eq)))
            .or(seq("<:".chars()).to(Token::Op(Op::Head)))
            .or(seq(":>".chars()).to(Token::Op(Op::Tail)))
            .or(seq("<=".chars()).to(Token::Op(Op::LessEq)))
            .or(seq(">=".chars()).to(Token::Op(Op::MoreEq)))
            .or(just('<').map(|sc| Token::Op(Op::Less)))
            .or(just('>').map(|sc| Token::Op(Op::More)))
            .or(just('!').map(|sc| Token::Op(Op::Not)))
            .or(just(',').map(|sc| Token::Comma))
            .or(just(':').map(|sc| Token::Colon))
            .map_with_range(|token, range| token.at(range.into()))
            .boxed();

        let tree = just('(').to(Delimiter::Paren).then(tokens.link()).padded_by(just(')'))
            .or(just('[').to(Delimiter::Brack).then(tokens.link()).padded_by(just(']')))
            .map_with_range(|(delim, tokens), range| Token::Tree(delim, tokens).at(range.into()));

        let token = number
            .or(string)
            .or(seq("true".chars()).map_with_range(|_, range| Token::Boolean(true).at(range.into())))
            .or(seq("false".chars()).map_with_range(|_, range| Token::Boolean(false).at(range.into())))
            .or(seq("null".chars()).map_with_range(|_, range| Token::Null.at(range.into())))
            .or(seq("let".chars()).map_with_range(|_, range| Token::Let.at(range.into())))
            .or(seq("if".chars()).map_with_range(|_, range| Token::If.at(range.into())))
            .or(seq("then".chars()).map_with_range(|_, range| Token::Then.at(range.into())))
            .or(seq("else".chars()).map_with_range(|_, range| Token::Else.at(range.into())))
            .or(seq("def".chars()).map_with_range(|_, range| Token::Def.at(range.into())))
            .or(seq("in".chars()).map_with_range(|_, range| Token::In.at(range.into())))
            .or(seq("of".chars()).map_with_range(|_, range| Token::Of.at(range.into())))
            .or(ident)
            .or(op)
            .or(tree)
            .padded_by(space.clone());

        space.padding_for(token.repeated())
    });

    tokens
        .padded_by(end())
        .parse(code.chars())
}
