#![feature(option_zip, trait_alias)]

#![allow(clippy::type_complexity)]

pub mod ast;
pub mod error;
pub mod node;
pub mod parse;
pub mod span;
pub mod src;
pub mod token;

pub use crate::{
    error::{Error, ErrorKind, Pattern},
    node::{Node, SrcNode},
    span::Span,
    src::SrcId,
    token::{Delimiter, Op, Token},
};

use chumsky::prelude::*;
use std::fmt;

fn parse<T>(parser: impl parse::Parser<T>, code: &str, src: SrcId) -> (Option<T>, Vec<Error>) {
    let mut errors = Vec::new();

    let len = code.chars().count();
    let eoi = Span::new(src, len..len);

    let (tokens, mut lex_errors) = token::lexer().parse_recovery(chumsky::Stream::from_iter(
        eoi,
        code.chars()
            .enumerate()
            .map(|(i, c)| (c, Span::new(src, i..i + 1))),
    ));
    errors.append(&mut lex_errors);

    let tokens = if let Some(tokens) = tokens {
        tokens
    } else {
        return (None, errors);
    };

    let (output, mut parse_errors) =
        parser.parse_recovery(chumsky::Stream::from_iter(eoi, tokens.into_iter()));
    errors.append(&mut parse_errors);

    (output, errors)
}

pub fn parse_expr(code: &str, src: SrcId) -> (Option<SrcNode<ast::Expr>>, Vec<Error>) {
    parse(
        parse::expr_parser()
            .then_ignore(end())
            .map_with_span(SrcNode::new),
        code,
        src,
    )
}

pub fn parse_module(code: &str, src: SrcId) -> (Option<SrcNode<ast::Module>>, Vec<Error>) {
    parse(
        parse::module_parser().map_with_span(SrcNode::new),
        code,
        src,
    )
}
