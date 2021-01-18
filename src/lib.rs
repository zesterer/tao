#![feature(
    label_break_value,
    option_zip,
    trait_alias,
    type_alias_impl_trait,
    arbitrary_self_types,
    option_expect_none,
)]

pub mod ast;
pub mod hir;
pub mod util;
pub mod error;

pub use self::{
    ast::{Loader, Src, Ident, lex},
    error::{ErrorCode, Error},
    util::Span,
};

use self::ast::LoadCache;

type Val = String;
type Ty = String;

pub fn eval_expr<L: Loader>(loader: L) {
    let mut cache = LoadCache::from(loader);
    let mut errors = Vec::new();
    'eval: {
        let (main, main_id) = match cache.load_from_path(std::iter::empty()) {
            Ok(src) => src,
            Err(e) => {
                errors.push(e);
                break 'eval;
            },
        };

        let (tokens, es) = lex(main_id, &main.code);
        errors.extend(es.into_iter());
        let tokens = match tokens {
            Some(tokens) => tokens,
            None => break 'eval,
        };

        //println!("Tokens: {:?}", tokens);

        let (ast, es) = ast::parse::parse_expr(main_id, &tokens);
        errors.extend(es.into_iter());
        let ast = match ast {
            Some(ast) => ast,
            None => break 'eval,
        };

        //println!("{:#?}", ast);

        let mut ctx = hir::Ctx::default();
        let hir = ast.to_hir(&mut ctx);
        errors.extend(std::mem::take(&mut ctx.errors).into_iter());

        //println!("{:#?}", hir);

        //println!("Expr: {}", ctx.display_expr(&hir));

        println!("Type: {}", ctx.display_ty(hir.ty()));
    }

    errors
        .into_iter()
        .for_each(|e| e.emit(&mut cache));
}

pub fn run_module<L: Loader>(loader: L) {
    eval_expr(loader)
}
