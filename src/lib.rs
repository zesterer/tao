#![type_length_limit = "10823821"]
#![feature(arbitrary_self_types, arbitrary_enum_discriminant)]

mod ast;
pub mod error;
mod hir;
mod lex;
mod mir;
mod node;
mod src;
mod ty;
pub mod vm;

use crate::{
    error::Error,
    node::SrcNode,
};
use internment::LocalIntern;

// TODO: Make this not hacky
fn parse_prelude() -> Result<SrcNode<ast::Module>, Vec<Error>> {
    ast::parse_module(&lex::lex(include_str!("tao/prelude.tao"))?)
}

pub fn run_module(src: &str) -> Result<Option<vm::Value>, Vec<Error>> {
    let tokens = lex::lex(&src)?;
    let mut ast = parse_prelude()?;
    ast.decls.append(&mut ast::parse_module(&tokens)?.decls);
    let hir_prog = hir::Program::new_root(&ast)?;

    // TODO: Get rid of this
    let main_ident = LocalIntern::new("main".to_string());

    let mir_prog = mir::Program::from_hir(&hir_prog, main_ident).map_err(|e| vec![e])?;

    let prog = mir_prog.compile(false).map_err(|e| vec![e])?;

    //println!("{:?}", prog);

    Ok(vm::Vm::default().execute(&prog))
}

pub fn run_expr(src: &str) -> Result<(ty::Type, vm::Value), Vec<Error>> {
    let tokens = lex::lex(&src)?;
    let mut hir_prog = hir::Program::new_root(&parse_prelude()?)?;
    hir_prog
        .insert_def(&ast::Def::main(ast::parse_expr(&tokens)?))
        .map_err(|e| vec![e])?;

    // TODO: Get rid of this
    let main_ident = LocalIntern::new("main".to_string());

    let mir_prog = mir::Program::from_hir(&hir_prog, main_ident).map_err(|e| vec![e])?;
    let prog = mir_prog.compile(true).map_err(|e| vec![e])?;

    Ok((
        hir_prog
            .root()
            .def(main_ident)
            .unwrap()
            .body
            .ty()
            .inner()
            .clone(),
        vm::Vm::default().execute(&prog).unwrap(),
    ))
}
