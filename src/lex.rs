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
    QuestionMark,
    Pipe,

    Let,
    If,
    Then,
    Else,
    Def,
    Given,
    In,
    Of,
    Type,
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
            Token::QuestionMark => write!(f, "?"),
            Token::Pipe => write!(f, "|"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Def => write!(f, "def"),
            Token::Given => write!(f, "given"),
            Token::In => write!(f, "in"),
            Token::Of => write!(f, "of"),
            Token::Type => write!(f, "type"),
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
            .map(|(mut int, fract)| {
                if let Some(mut fract) = fract {
                    int.push('.');
                    int.append(&mut fract);
                }
                Token::Number(LocalIntern::new(int.into_iter().collect()))
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
            .map(|chars| Token::String(LocalIntern::new(chars.into_iter().collect())))
            .boxed();

        let ident = permit(|c: &char| c.is_ascii_alphabetic() || *c == '_').once_or_more()
            .map(|chars| Token::Ident(LocalIntern::new(chars.into_iter().collect())));

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
            .or(just('?').map(|sc| Token::QuestionMark))
            .or(just('|').map(|sc| Token::Pipe))
            .boxed();

        let tree = just('(').to(Delimiter::Paren).then(tokens.link()).padded_by(just(')'))
            .or(just('[').to(Delimiter::Brack).then(tokens.link()).padded_by(just(']')))
            .map(|(delim, tokens)| Token::Tree(delim, tokens));

        let token = number
            .or(string)
            .or(seq("true".chars()).to(Token::Boolean(true)))
            .or(seq("false".chars()).to(Token::Boolean(false)))
            .or(seq("null".chars()).to(Token::Null))
            .or(seq("let".chars()).to(Token::Let))
            .or(seq("if".chars()).to(Token::If))
            .or(seq("then".chars()).to(Token::Then))
            .or(seq("else".chars()).to(Token::Else))
            .or(seq("def".chars()).to(Token::Def))
            .or(seq("given".chars()).to(Token::Given))
            .or(seq("in".chars()).to(Token::In))
            .or(seq("of".chars()).to(Token::Of))
            .or(seq("type".chars()).to(Token::Type))
            .or(ident)
            .or(op)
            .or(tree)
            .map_with_region(|token, region| token.at(region))
            .padded_by(space.clone());

        space.padding_for(token.repeated())
    });

    tokens
        .padded_by(end())
        .parse(code.chars())
}
