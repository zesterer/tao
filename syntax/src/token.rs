use super::*;
use chumsky::prelude::*;
use internment::Intern;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Paren,
    Brack,
    Brace,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    // Sum
    Add, Sub,
    // Product
    Mul, Div, Rem,
    // Equality
    Eq, NotEq,
    // Comparison
    Less, LessEq,
    More, MoreEq,
    // Logical
    Not, And, Or, Xor,
    // Lists
    Join,
    Ellipsis,
    Dot,
    RArrow,
    RFlow,
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
            Op::LessEq => write!(f, "<="),
            Op::More => write!(f, ">"),
            Op::MoreEq => write!(f, ">="),
            Op::Not => write!(f, "!"),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::Xor => write!(f, "xor"),
            Op::Join => write!(f, "++"),
            Op::Dot => write!(f, "."),
            Op::Ellipsis => write!(f, ".."),
            Op::RArrow => write!(f, "->"),
            Op::RFlow => write!(f, "=>"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Hash, Eq)]
pub enum Token {
    Nat(u64),
    Num(Intern<String>),
    Char(char),
    Bool(bool),
    Str(Intern<String>),
    Open(Delimiter),
    Close(Delimiter),
    Op(Op),
    TermIdent(ast::Ident),
    TypeIdent(ast::Ident),
    Comma,
    Separator,
    Colon,
    Wildcard,
    Question,
    Pipe,
    Data,
    Type,
    Def,
    Fn,
    Let,
    If,
    Match,
    Then,
    Else,
    In,
    Tilde,
    Dollar,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Nat(x) => write!(f, "{}", x),
            Token::Num(x) => write!(f, "{}", x),
            Token::Char(c) => write!(f, "{}", c),
            Token::Bool(x) => write!(f, "{}", x),
            Token::Str(s) => write!(f, "{}", s),
            Token::Open(Delimiter::Paren) => write!(f, "("),
            Token::Open(Delimiter::Brack) => write!(f, "["),
            Token::Open(Delimiter::Brace) => write!(f, "{{"),
            Token::Close(Delimiter::Paren) => write!(f, ")"),
            Token::Close(Delimiter::Brack) => write!(f, "]"),
            Token::Close(Delimiter::Brace) => write!(f, "}}"),
            Token::Op(op) => write!(f, "{}", op),
            Token::TermIdent(ident) => write!(f, "{}", ident),
            Token::TypeIdent(ident) => write!(f, "{}", ident),
            Token::Comma => write!(f, ","),
            Token::Separator => write!(f, "::"),
            Token::Colon => write!(f, ":"),
            Token::Wildcard => write!(f, "_"),
            Token::Question => write!(f, "?"),
            Token::Pipe => write!(f, "|"),
            Token::Data => write!(f, "data"),
            Token::Type => write!(f, "type"),
            Token::Def => write!(f, "def"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Match => write!(f, "match"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::In => write!(f, "in"),
            Token::Tilde => write!(f, "~"),
            Token::Dollar => write!(f, "$"),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Error> {
    let nat = text::int(10)
        .map(|s: String| Token::Nat(s.parse().unwrap()));

    let num = text::int(10)
        .then_ignore(just('.'))
        .chain::<char, _, _>(text::digits(10))
        .collect::<String>()
        .map(Intern::new)
        .map(Token::Num);

    let ctrl = just(',').to(Token::Comma)
        .or(seq("::".chars()).to(Token::Separator))
        .or(just(':').to(Token::Colon))
        .or(just('?').to(Token::Question))
        .or(just('|').to(Token::Pipe))
        .or(just('~').to(Token::Tilde))
        .or(just('$').to(Token::Dollar));

    let op = seq("=>".chars()).to(Op::RFlow)
        .or(just('=').to(Op::Eq))
        .or(seq("..".chars()).to(Op::Ellipsis))
        .or(just('.').to(Op::Dot))
        .or(seq("!=".chars()).to(Op::NotEq))
        .or(just('!').to(Op::Not))
        .or(seq("<=".chars()).to(Op::LessEq))
        .or(just('<').to(Op::Less))
        .or(seq(">=".chars()).to(Op::MoreEq))
        .or(just('>').to(Op::More))
        .or(seq("++".chars()).to(Op::Join))
        .or(just('+').to(Op::Add))
        .or(seq("->".chars()).to(Op::RArrow))
        .or(just('-').to(Op::Sub))
        .or(just('*').to(Op::Mul))
        .or(just('/').to(Op::Div))
        .or(just('%').to(Op::Rem))
        .or(just('=').to(Op::Eq))
        .map(Token::Op);

    let delim = just('(').to(Token::Open(Delimiter::Paren))
        .or(just(')').to(Token::Close(Delimiter::Paren)))
        .or(just('[').to(Token::Open(Delimiter::Brack)))
        .or(just(']').to(Token::Close(Delimiter::Brack)))
        .or(just('{').to(Token::Open(Delimiter::Brace)))
        .or(just('}').to(Token::Close(Delimiter::Brace)));

    let escape = just('\\')
        .ignore_then(just('\\')
        .or(just('/'))
        .or(just('"'))
        .or(just('b').to('\x08'))
        .or(just('f').to('\x0C'))
        .or(just('n').to('\n'))
        .or(just('r').to('\r'))
        .or(just('t').to('\t')));

    let string = just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Intern::new)
        .map(Token::Str)
        .labelled("string");

    let word = text::ident().map(|s: String| match s.as_str() {
        "data" => Token::Data,
        "type" => Token::Type,
        "def" => Token::Def,
        "fn" => Token::Fn,
        "let" => Token::Let,
        "if" => Token::If,
        "match" => Token::Match,
        "then" => Token::Then,
        "else" => Token::Else,
        "in" => Token::In,
        "and" => Token::Op(Op::And),
        "or" => Token::Op(Op::Or),
        "xor" => Token::Op(Op::Xor),
        "True" => Token::Bool(true),
        "False" => Token::Bool(false),
        "_" => Token::Wildcard,
        _ => if s.chars().next().map_or(false, |c| c.is_uppercase()) {
            Token::TypeIdent(ast::Ident::new(s))
        } else {
            Token::TermIdent(ast::Ident::new(s))
        },
    });

    let comments = just('#')
        .then_ignore(none_of("}".chars())
            .ignored()
            .repeated()
            .delimited_by('{', '}')
            .or(none_of("\n".chars()).ignored().repeated()))
        .padded()
        .ignored()
        .repeated();

    let token = ctrl
        .or(word)
        .or(num)
        .or(nat)
        .or(op)
        .or(delim)
        .or(string)
        .recover_with(skip_then_retry_until([]))
        .map_with_span(move |token, span| (token, span))
        .padded();

    token
        .padded_by(comments)
        .repeated()
        .padded()
        .then_ignore(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let code = "+ - *+++/++++hello)Hello[}!>>=42and";
        let len = code.chars().count();

        let span = |i| Span::new(SrcId::empty(), i..i + 1);

        assert_eq!(
            lexer()
                .parse(chumsky::Stream::from_iter(
                    span(len),
                    code.chars().enumerate().map(|(i, c)| (c, span(i))),
                ))
                .map(|tokens| tokens.into_iter().map(|(tok, _)| tok).collect::<Vec<_>>()),
            Ok(vec![
                Token::Op(Op::Add),
                Token::Op(Op::Sub),
                Token::Op(Op::Mul),
                Token::Op(Op::Join),
                Token::Op(Op::Add),
                Token::Op(Op::Div),
                Token::Op(Op::Join),
                Token::Op(Op::Join),
                Token::TermIdent(ast::Ident::new("hello")),
                Token::Close(Delimiter::Paren),
                Token::TypeIdent(ast::Ident::new("Hello")),
                Token::Open(Delimiter::Brack),
                Token::Close(Delimiter::Brace),
                Token::Op(Op::Not),
                Token::Op(Op::More),
                Token::Op(Op::MoreEq),
                Token::Nat(42),
                Token::Op(Op::And),
            ]),
        );
    }
}
