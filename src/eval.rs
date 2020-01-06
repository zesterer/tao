use crate::parse::{Value, Expr};

#[derive(Debug)]
pub enum EvalError {}

pub fn eval(expr: &Expr) -> Result<Value, EvalError> {
    match expr {
        Expr::Literal(x) => Ok(x.clone()),
        Expr::Ident(_) => todo!(),
    }
}
