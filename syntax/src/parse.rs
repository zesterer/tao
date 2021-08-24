use super::*;
use chumsky::{prelude::*, Span as _};

pub trait Parser<T> = chumsky::Parser<Token, T, Error = Simple<Token, Span>> + Clone;

pub fn literal_parser() -> impl Parser<ast::Literal> {
    filter_map(|span, token| match token {
        Token::Nat(x) => Ok(ast::Literal::Nat(x)),
        Token::Num(x) => Ok(ast::Literal::Num(x)),
        Token::Bool(x) => Ok(ast::Literal::Bool(x)),
        Token::Char(x) => Ok(ast::Literal::Char(x)),
        Token::Str(x) => Ok(ast::Literal::Str(x)),
        token => Err(Simple::expected_token_found(Some(span), Vec::new(), Some(token))),
    })
        .labelled("literal")
}

pub fn term_ident_parser() -> impl Parser<ast::Ident> {
    filter_map(|span, token| match token {
        Token::TermIdent(x) => Ok(x),
        token => Err(Simple::expected_token_found(Some(span), Vec::new(), Some(token))),
    })
        .labelled("identifier")
}

pub fn type_ident_parser() -> impl Parser<ast::Ident> {
    filter_map(|span, token| match token {
        Token::TypeIdent(x) => Ok(x),
        token => Err(Simple::expected_token_found(Some(span), Vec::new(), Some(token))),
    })
        .labelled("type name")
}

pub fn nat_parser() -> impl Parser<u64> {
    filter_map(|span, token| match token {
        Token::Nat(x) => Ok(x),
        token => Err(Simple::expected_token_found(Some(span), Vec::new(), Some(token))),
    })
        .labelled("natural")
}

pub fn bool_parser() -> impl Parser<bool> {
    filter_map(|span, token| match token {
        Token::Bool(x) => Ok(x),
        token => Err(Simple::expected_token_found(Some(span), Vec::new(), Some(token))),
    })
        .labelled("boolean")
}

pub fn nested_parser<T>(parser: impl Parser<T>, delimiter: Delimiter, f: impl Fn() -> T + Clone) -> impl Parser<T> {
    parser
        .delimited_by(Token::Open(delimiter), Token::Close(delimiter))
        .map(move |x| x.unwrap_or_else(&f))
}

pub fn expr_parser() -> impl Parser<SrcNode<ast::Expr>> {
    recursive::<_, SrcNode<ast::Expr>, _, _, _>(|expr| {
        let litr = literal_parser().map(ast::Expr::Literal);
        let ident = term_ident_parser().map(ast::Expr::Local);

        let atom = litr
            .or(ident)
            .or(nested_parser(expr.map(|expr| expr.into_inner()), Delimiter::Paren, || ast::Expr::Error))
            .map_with_span(|x, span| SrcNode::new(x, span.unwrap()));

        let op = just(Token::Op(Op::Sub)).to(ast::UnaryOp::Neg)
            .or(just(Token::Op(Op::Not)).to(ast::UnaryOp::Not))
            .map_with_span(|op, span| SrcNode::new(op, span.unwrap()));
        let unary = op.repeated()
            .then(atom)
            .foldr(|op, expr| {
                let span = op.span().union(expr.span());
                SrcNode::new(ast::Expr::Unary(op, expr), span)
            });

        // Product
        let op = just(Token::Op(Op::Mul)).to(ast::BinaryOp::Mul)
            .or(just(Token::Op(Op::Div)).to(ast::BinaryOp::Div))
            .or(just(Token::Op(Op::Rem)).to(ast::BinaryOp::Rem))
            .map_with_span(|x, span| SrcNode::new(x, span.unwrap()));
        let product = unary.clone()
            .then(op.then(unary).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            });

        // Sum
        let op = just(Token::Op(Op::Add)).to(ast::BinaryOp::Add)
            .or(just(Token::Op(Op::Sub)).to(ast::BinaryOp::Sub))
            .map_with_span(|x, span| SrcNode::new(x, span.unwrap()));
        let sum = product.clone()
            .then(op.then(product).repeated())
            .foldl(|a, (op, b)| {
                let span = a.span().union(b.span());
                SrcNode::new(ast::Expr::Binary(op, a, b), span)
            });

        sum
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::Parser;

    #[test]
    fn simple() {
        let code = r#"
            4 * 5 + (3 - 2) / 3
        "#;

        let tokens = token::lexer(SrcId::empty())
            .parse(code)
            .unwrap();

        let res = expr_parser().parse(tokens).unwrap();
        assert_eq!(
            res,
            SrcNode::new(ast::Expr::Binary(
                SrcNode::new(ast::BinaryOp::Add, Span::empty()),
                SrcNode::new(ast::Expr::Binary(
                    SrcNode::new(ast::BinaryOp::Mul, Span::empty()),
                    SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(4)), Span::empty()),
                    SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(5)), Span::empty()),
                ), Span::empty()),
                SrcNode::new(ast::Expr::Binary(
                    SrcNode::new(ast::BinaryOp::Div, Span::empty()),
                    SrcNode::new(ast::Expr::Binary(
                        SrcNode::new(ast::BinaryOp::Sub, Span::empty()),
                        SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(3)), Span::empty()),
                        SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(2)), Span::empty()),
                    ), Span::empty()),
                    SrcNode::new(ast::Expr::Literal(ast::Literal::Nat(3)), Span::empty()),
                ), Span::empty()),
            ), Span::empty()),
            "{:#?}", res,
        );
    }
}
