#![feature(
    label_break_value,
    option_zip,
    arbitrary_self_types,
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

use ariadne::FnCache;

type Val = String;
type Ty = String;

impl ariadne::Span for Span {
    type SourceId = SrcId;

    fn source(&self) -> SrcId { self.src().unwrap() }
    fn start(&self) -> usize { self.range().unwrap().start }
    fn end(&self) -> usize { self.range().unwrap().end }
}

pub fn eval_expr<L: Loader>(loader: L) {
    let mut cache = FnCache::new(|src_id: &SrcId| Ok(loader.load(*src_id)?.code));

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

        // println!("{:#?}", ast);

        let mut ctx = hir::Ctx::default();
        let hir = ast.to_hir(&mut ctx);
        errors.extend(ctx.take_errors().into_iter());

        //println!("{:#?}", hir);

        //println!("Expr: {}", ctx.display_expr(&hir));

        println!("Type: {}", ctx.display_ty(hir.ty()));
    }

    errors
        .into_iter()
        .for_each(|e| e.emit(&mut cache));
}

pub fn eval_prog<L: Loader>(loader: L) {
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

        let (ast, es) = ast::parse::parse_module(main_id, &tokens);
        errors.extend(es.into_iter());
        let ast = match ast {
            Some(ast) => ast,
            None => break 'eval,
        };

        let prog = ast::Program::new(ast);

        println!("{:#?}", prog);

        let hir = match prog.to_hir() {
            Ok(hir) => hir,
            Err(errs) => {
                errors.extend(errs.into_iter());
                break 'eval;
            },
        };

        println!("{:#?}", hir);

        //println!("Expr: {}", ctx.display_expr(&hir));

        // println!("Type: {}", ctx.display_ty(hir.ty()));
    }

    errors
        .into_iter()
        .for_each(|e| e.emit(&mut cache));
}
