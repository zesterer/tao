use std::{
    fmt,
    cmp::PartialEq,
};
use parze::prelude::*;
use internment::LocalIntern;
use crate::{
    src::Span,
    node::SrcNode,
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
    NotEq,
    Less,
    More,
    LessEq,
    MoreEq,

    And,
    Or,

    Join,
    Ellipsis,

    Not,
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
            Op::NotEq => write!(f, "!="),
            Op::Less => write!(f, "<"),
            Op::More => write!(f, ">"),
            Op::LessEq => write!(f, "<="),
            Op::MoreEq => write!(f, ">="),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Join => write!(f, "++"),
            Op::Ellipsis => write!(f, "..."),
            Op::Not => write!(f, "!"),
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
    Char(char),
    String(LocalIntern<String>),
    Null,
    Ident(LocalIntern<String>),
    TypeName(LocalIntern<String>),
    Op(Op),
    Tree(Delimiter, Vec<SrcNode<Token>>),

    RArrow,
    RMap,
    Comma,
    Colon,
    Dot,
    QuestionMark,
    Pipe,
    Dollar,
    Separator,
    Wildcard,

    Let,
    If,
    Match,
    Then,
    Else,
    Def,
    Fn,
    Given,
    In,
    As,
    With,
    Of,
    Type,
    Data,
}

impl Token {
    fn at(self, span: Span) -> SrcNode<Self> {
        SrcNode::new(self, span)
    }
}

impl PartialEq<Token> for SrcNode<Token> {
    fn eq(&self, other: &Token) -> bool {
        &**self == other
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Number(x) => write!(f, "{}", x),
            Token::Boolean(x) => write!(f, "{}", x),
            Token::Char(c) => write!(f, "'{}'", c),
            Token::String(x) => write!(f, "\"{}\"", x),
            Token::Null => write!(f, "null"),
            Token::Ident(i) => write!(f, "{}", i),
            Token::TypeName(i) => write!(f, "{}", i),
            Token::Op(op) => write!(f, "{}", op),
            Token::Tree(delim, tokens) => write!(f, "{}...{}", delim.left(), delim.right()),
            Token::RArrow => write!(f, "->"),
            Token::RMap => write!(f, "=>"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Dot => write!(f, "."),
            Token::QuestionMark => write!(f, "?"),
            Token::Pipe => write!(f, "|"),
            Token::Dollar => write!(f, "$"),
            Token::Separator => write!(f, "::"),
            Token::Wildcard => write!(f, "_"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Match => write!(f, "match"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Def => write!(f, "def"),
            Token::Fn => write!(f, "fn"),
            Token::Given => write!(f, "given"),
            Token::In => write!(f, "in"),
            Token::As => write!(f, "as"),
            Token::With => write!(f, "with"),
            Token::Of => write!(f, "of"),
            Token::Type => write!(f, "type"),
            Token::Data => write!(f, "data"),
        }
    }
}

pub fn lex(code: &str) -> Result<Vec<SrcNode<Token>>, Vec<Error>> {
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

        let char_inner = permit(|c: &char| *c != '\\' && *c != '"').or(escape);

        let character = just('\'')
            .padding_for(char_inner.clone())
            .padded_by(just('\''))
            .map(Token::Char)
            .boxed();

        let string = just('"')
            .padding_for(char_inner.repeated())
            .padded_by(just('"'))
            .map(|chars| Token::String(LocalIntern::new(chars.into_iter().collect())))
            .boxed();

        let ident_raw = permit(|c: &char| c.is_ascii_lowercase() || *c == '_')
            .then(permit(|c: &char| c.is_ascii_alphanumeric() || *c == '_').repeated())
            .map(|(head, tail)| std::iter::once(head).chain(tail.into_iter()).collect());

        let type_name_raw = permit(char::is_ascii_uppercase)
            .then(permit(|c: &char| c.is_ascii_alphanumeric() || *c == '_').repeated())
            .map(|(head, tail)| std::iter::once(head).chain(tail.into_iter()).collect());

        // `$` reverses the ident/type name

        let ident = ident_raw.clone()
            .or(just('$').padding_for(type_name_raw.clone()));

        let type_name = type_name_raw
            .or(just('$').padding_for(ident_raw));

        let op = seq("->".chars()).to(Token::RArrow)
            .or(seq("=>".chars()).to(Token::RMap))
            .or(seq("++".chars()).to(Token::Op(Op::Join)))
            .or(seq("...".chars()).to(Token::Op(Op::Ellipsis)))
            .or(just('+').to(Token::Op(Op::Add)))
            .or(just('-').to(Token::Op(Op::Sub)))
            .or(just('*').to(Token::Op(Op::Mul)))
            .or(just('/').to(Token::Op(Op::Div)))
            .or(just('%').to(Token::Op(Op::Rem)))
            .or(just('=').to(Token::Op(Op::Eq)))
            .boxed()
            .or(seq("!=".chars()).to(Token::Op(Op::NotEq)))
            .or(seq("<=".chars()).to(Token::Op(Op::LessEq)))
            .or(seq(">=".chars()).to(Token::Op(Op::MoreEq)))
            .boxed()
            .or(just('<').map(|sc| Token::Op(Op::Less)))
            .or(just('>').map(|sc| Token::Op(Op::More)))
            .or(just('!').map(|sc| Token::Op(Op::Not)))
            .or(just(',').map(|sc| Token::Comma))
            .or(seq("::".chars()).map(|sc| Token::Separator))
            .or(just(':').map(|sc| Token::Colon))
            .or(just('.').map(|sc| Token::Dot))
            .or(just('?').map(|sc| Token::QuestionMark))
            .or(just('|').map(|sc| Token::Pipe))
            .or(just('$').map(|sc| Token::Dollar))
            .boxed();

        let tree = just('(').to(Delimiter::Paren).then(tokens.link()).padded_by(just(')'))
            .or(just('[').to(Delimiter::Brack).then(tokens.link()).padded_by(just(']')))
            .or(just('{').to(Delimiter::Brace).then(tokens.link()).padded_by(just('}')))
            .map(|(delim, tokens)| Token::Tree(delim, tokens));

        let token = number
            .or(character)
            .or(string)
            .or(type_name.map(|s| Token::TypeName(LocalIntern::new(s))))
            .or(ident.map(|s: String| match s.as_str() {
                "_" => Token::Wildcard,
                "true" => Token::Boolean(true),
                "false" => Token::Boolean(false),
                "null" => Token::Null,
                "let" => Token::Let,
                "if" => Token::If,
                "match" => Token::Match,
                "then" => Token::Then,
                "else" => Token::Else,
                "def" => Token::Def,
                "fn" => Token::Fn,
                "given" => Token::Given,
                "in" => Token::In,
                "as" => Token::As,
                "with" => Token::With,
                "of" => Token::Of,
                "type" => Token::Type,
                "data" => Token::Data,
                "and" => Token::Op(Op::And),
                "or" => Token::Op(Op::Or),
                _ => Token::Ident(LocalIntern::new(s)),
            }))
            .or(op)
            .or(tree)
            .map_with_span(|token, span| token.at(span))
            .padded_by(space.clone());

        space.padding_for(token.repeated())
    });

    tokens
        .padded_by(end())
        .parse(code.chars())
}
