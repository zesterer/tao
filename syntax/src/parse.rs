use super::*;
use chumsky::prelude::*;

pub fn parse_literal() -> impl Parser<Token, SrcNode<ast::Literal>, Error = Simple<Token, Span>> {
    filter_map(|span, token| match token {
        Token::Nat(x) => Ok(ast::Literal::Nat(x)),
        Token::Num(x) => Ok(ast::Literal::Num(x)),
        Token::Char(x) => Ok(ast::Literal::Char(x)),
        Token::Str(x) => Ok(ast::Literal::Str(x)),
        token => Err(Simple::expected_token_found(Some(span), Vec::new(), Some(token))),
    })
        .labelled("literal")
        .map_with_span(|x, span| SrcNode::new(x, span.unwrap()))
}
