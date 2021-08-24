use super::*;
use chumsky::prelude::*;
use internment::Intern;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Delimiter {
    Paren,
    Brack,
    Brace,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
    Nat(u64),
    Num(f64),
    Char(char),
    Bool(bool),
    Str(Intern<String>),
    Open(Delimiter),
    Close(Delimiter),
    Op(Op),
    TermIdent(ast::Ident),
    TypeIdent(ast::Ident),
}

pub fn lexer(src: SrcId) -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let nat = text::int()
        .collect::<String>()
        .map(|s| Token::Nat(s.parse().unwrap()));

    let op = just('=').to(Op::Eq)
        .or(just('!')
            .padding_for(just('=').or_not())
            .map(|x| if x.is_some() { Op::NotEq } else { Op::Not }))
        .or(just('+')
            .padding_for(just('+').or_not())
            .map(|x| if x.is_some() { Op::Join } else { Op::Add }))
        .or(just('<')
            .padding_for(just('=').or_not())
            .map(|x| if x.is_some() { Op::LessEq } else { Op::Less }))
        .or(just('>')
            .padding_for(just('=').or_not())
            .map(|x| if x.is_some() { Op::MoreEq } else { Op::More }))
        .or(just('-').to(Op::Sub))
        .or(just('*').to(Op::Mul))
        .or(just('/').to(Op::Div))
        .or(just('%').to(Op::Rem))
        .map(Token::Op);

    let delim = just('(').to(Token::Open(Delimiter::Paren))
        .or(just(')').to(Token::Close(Delimiter::Paren)))
        .or(just('[').to(Token::Open(Delimiter::Brack)))
        .or(just(']').to(Token::Close(Delimiter::Brack)))
        .or(just('{').to(Token::Open(Delimiter::Brace)))
        .or(just('}').to(Token::Close(Delimiter::Brace)));

    let word = text::ident()
        .collect::<String>()
        .map(|s| match s.as_str() {
            "and" => Token::Op(Op::And),
            "or" => Token::Op(Op::Or),
            "xor" => Token::Op(Op::Xor),
            "True" => Token::Bool(true),
            "False" => Token::Bool(false),
            _ => if s.chars().next().map_or(false, |c| c.is_uppercase()) {
                Token::TypeIdent(ast::Ident::new(s))
            } else {
                Token::TermIdent(ast::Ident::new(s))
            },
        });

    let token = nat
        .or(op)
        .or(delim)
        .or(word)
        .map_with_span(move |token, range| (token, Span::new(src, range)))
        .padded();

    token
        .repeated()
        .padded_by(end())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        assert_eq!(
            lexer(SrcId::empty())
                .parse("+ - *+++/++++hello)Hello[}!>>=42and")
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
