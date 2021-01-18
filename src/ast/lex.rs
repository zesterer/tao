use internment::Intern;
// use parze::{
//     prelude::*,
//     Error as ParzeError,
// };
use lingo::{prelude::*, Span as LingoSpan};
use crate::{
    util::{Span, SrcNode},
    ErrorCode, Error,
};
use super::SrcId;
use std::{fmt, ops::Range, marker::PhantomData};

pub enum LexError {
    Unexpected(char, Range<usize>, Option<char>),
    UnexpectedEnd,
}

struct LexContext;

impl Context for LexContext {
    type Token = char;
    type Span = Range<usize>;
    type Error = LexError;
}

impl lingo::Error<LexContext> for LexError {
    type Item = char;

    fn span(&self) -> Option<Range<usize>> {
        Some(match self {
            LexError::Unexpected(_, span, _) => span.clone(),
            LexError::UnexpectedEnd => Range::end(),
        })
    }

    fn unexpected_end() -> Self { LexError::UnexpectedEnd }

    fn unexpected(found: Self::Item, span: Range<usize>, expected: Option<Self::Item>) -> Self {
        LexError::Unexpected(found, span, expected)
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum Op {
    Add, Sub,
    Mul, Div, Rem,

    Eq, NotEq,
    Less, LessEq,
    More, MoreEq,

    And, Or, Not,

    Join,

    Ellipsis,
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

#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    Nat(Intern<String>),
    Int(Intern<String>),
    Num(Intern<String>),
    Boolean(bool),
    Char(char),
    String(Intern<String>),
    Null,
    Ident(Intern<String>),
    Intrinsic(Intern<String>),
    TypeName(Intern<String>),
    Op(Op),
    Tree(Delimiter, Vec<SrcNode<Token>>),

    LArrow,
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
    Semicolon,
    Ampersand,

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
    Do,
    Return,

    Root,
    This,
    Super,

    Poison,
}

impl<'a> From<&'a Token> for Token {
    fn from(tok: &'a Token) -> Self {
        tok.clone()
    }
}

impl<'a> PartialEq<&'a Token> for Token {
    fn eq(&self, other: &&'a Token) -> bool {
        self == *other
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Nat(x) => write!(f, "{}", x),
            Token::Int(x) => write!(f, "{}", x),
            Token::Num(x) => write!(f, "{}", x),
            Token::Boolean(x) => write!(f, "{}", x),
            Token::Char(c) => write!(f, "'{}'", c),
            Token::String(x) => write!(f, "\"{}\"", x),
            Token::Null => write!(f, "null"),
            Token::Ident(i) => write!(f, "{}", i),
            Token::Intrinsic(i) => write!(f, "@{}", i),
            Token::TypeName(i) => write!(f, "{}", i),
            Token::Op(op) => write!(f, "{}", op),
            Token::Tree(delim, tokens) => write!(f, "{}...{}", delim.left(), delim.right()),
            Token::LArrow => write!(f, "<-"),
            Token::RArrow => write!(f, "->"),
            Token::RMap => write!(f, "=>"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Dot => write!(f, "."),
            Token::QuestionMark => write!(f, "?"),
            Token::Pipe => write!(f, "|"),
            Token::Dollar => write!(f, "$"),
            Token::Separator => write!(f, "::"),
            Token::Semicolon => write!(f, ";"),
            Token::Wildcard => write!(f, "_"),
            Token::Semicolon => write!(f, ";"),
            Token::Ampersand => write!(f, "&"),
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
            Token::Do => write!(f, "do"),
            Token::Return => write!(f, "return"),
            Token::Root => write!(f, "root"),
            Token::Super => write!(f, "super"),
            Token::This => write!(f, "this"),
            Token::Poison => write!(f, "<poison>"),
        }
    }
}

impl Token {
    fn at(self, span: Span) -> SrcNode<Self> {
        SrcNode::new(self, span)
    }
}

impl<'a> PartialEq<&'a SrcNode<Token>> for Token {
    fn eq(&self, other: &&'a SrcNode<Token>) -> bool {
        self == &***other
    }
}

impl<'a> From<&'a SrcNode<Token>> for Token {
    fn from(token: &'a SrcNode<Token>) -> Self {
        token.inner().clone()
    }
}

pub fn lex(src_id: SrcId, code: &str) -> (Option<Vec<SrcNode<Token>>>, Vec<Error>) {
    let tokens = recursive::<LexContext, _, _, _>(move |tokens| {
        let whitespace = permit(|c: &char| c.is_whitespace()).to(())
            .or(just('#').padding_for(permit(|c: &char| *c != '\n').repeated()).to(()));

        let space = whitespace.repeated();

        let digits = permit::<LexContext, _>(|c: &char| c.is_ascii_digit()).once_or_more();

        let natural = digits.clone()
            .map(|digits| Token::Nat(Intern::new(digits.into_iter().collect())))
            .boxed();

        let number = digits.clone()
            .then(just('.').padding_for(digits))
            .map(|(mut int, mut fract)| {
                int.push('.');
                int.append(&mut fract);
                Token::Num(Intern::new(int.into_iter().collect()))
            })
            .boxed();

        let special = just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .boxed();
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
            .map(|chars| Token::String(Intern::new(chars.into_iter().collect())))
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

        let op = seq("<-".chars()).to(Token::LArrow)
            .or(seq("->".chars()).to(Token::RArrow))
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
            .or(just('<').to(Token::Op(Op::Less)))
            .or(just('>').to(Token::Op(Op::More)))
            .or(just('!').to(Token::Op(Op::Not)))
            .or(just(',').to(Token::Comma))
            .or(seq("::".chars()).to(Token::Separator))
            .or(just(';').to(Token::Semicolon))
            .or(just('&').to(Token::Ampersand))
            .or(just(':').to(Token::Colon))
            .or(just('.').to(Token::Dot))
            .or(just('?').to(Token::QuestionMark))
            .or(just('|').to(Token::Pipe))
            .or(just('$').to(Token::Dollar))
            .boxed();

        let tree = just('(').to(Delimiter::Paren).then(tokens.clone().padded_by(just(')')))
            .or(just('[').to(Delimiter::Brack).then(tokens.clone()).padded_by(just(']')))
            .or(just('{').to(Delimiter::Brace).then(tokens.clone()).padded_by(just('}')))
            .map(|(delim, tokens)| Token::Tree(delim, tokens));

        let token = number
            .or(natural)
            .or(character)
            .or(string)
            .or(type_name.map(|s| Token::TypeName(Intern::new(s))))
            .or(just('@')
                .padding_for(ident.clone())
                .map(|s| Token::Intrinsic(Intern::new(s))))
            .boxed()
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
                "do" => Token::Do,
                "return" => Token::Return,
                "root" => Token::Root,
                "self" => Token::This,
                "super" => Token::Super,
                _ => Token::Ident(Intern::new(s)),
            }))
            .or(op)
            .or(tree)
            // .recover(none_of("])}".chars()).to(Token::Poison))
            .map_with_span(move |token, span| token.at(Span::new(src_id, span)))
            .padded_by(space.clone());

        space
            .padding_for(token.repeated())
            .map(|tokens| tokens
                .into_iter()
                .filter(|t| !matches!(t.inner(), Token::Poison))
                .collect())
    });

    let (tokens, errors) = tokens
        .padded_by(end())
        .parse(Stream::from_iter(code
            .chars()
            .enumerate()
            .map(|(i, c)| (c, i..i + 1))));

    let errors = errors
        .into_iter()
        .map(|e| match e {
            LexError::Unexpected(c, span, expected) => {
                let span = Span::new(src_id, span);
                Error::new(
                    ErrorCode::UnexpectedChar,
                    span,
                    if let Some(expected) = expected {
                        format!("Unexpected character `{}`, expected `{}`", c, expected)
                    } else {
                        format!("Unexpected character `{}`", c)
                    },
                )
                    .with_primary(span, None)
            },
            LexError::UnexpectedEnd => Error::new(
                ErrorCode::UnexpectedEnd,
                Span::none().with_src(src_id),
                format!("Unexpected end of input"),
            ),
        })
        .collect();

    (tokens, errors)
}

// impl ParzeError<char> for LexError {
//     type Span = Range<usize>;

//     fn position(&self) -> Option<usize> {
//         match self {
//             LexError::Unexpected(_, x) => Some(*x),
//             _ => None,
//         }
//     }

//     fn unexpected_end() -> Self {
//         LexError::UnexpectedEnd
//     }

//     fn unexpected(span: Self::Span, f: char) -> Self {
//         LexError::Unexpected(f, span)
//     }
// }
