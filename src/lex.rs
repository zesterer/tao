use parze::prelude::*;
use internment::Intern;

#[derive(Copy, Clone, Debug)]
pub enum Token {
    Number(f64),
    Bool(bool),
    Ident(Intern<String>),
    String(Intern<String>),
}

pub fn lex(code: &str) -> Result<Vec<Token>, DefaultError<char>> {
    let integer = permit(|c: &char| c.is_ascii_digit()).once_or_more();

    let number = integer.clone()
        .then(just('.').padding_for(integer).or_not())
        .map(|(mut head, fract)| {
            if let Some(mut fract) = fract {
                head.push('.');
                head.append(&mut fract);
            }
            Token::Number(head.into_iter().collect::<String>().parse().unwrap())
        });

    let token = number.then(permit(|c: &char| c.is_whitespace()).repeated()).map(|(t, _)| t);

    let tokens = token.repeated()
        .then(end()).map(|(ts, _)| ts);

    tokens
        .parse(code.chars())
        .map_err(|mut errs| errs.remove(0))
}
