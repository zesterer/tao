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

use crate::error::Error;
use internment::LocalIntern;

pub fn run_module(src: &str) -> Result<vm::Value, Vec<Error>> {
    let tokens = lex::lex(&src)?;
    let ast = ast::parse_module(&tokens)?;
    let hir_prog = hir::Program::new_root(&ast)?;

    //println!("{:?}", hir_prog.root().def(LocalIntern::new("find_char".to_string())));

    // TODO: Get rid of this
    let main_ident = LocalIntern::new("main".to_string());
    if let Some(main) = hir_prog.root().def(main_ident) {
        println!("TYPE: {}", **main.body.ty());
    }

    let mir_prog = mir::Program::from_hir(&hir_prog, main_ident).map_err(|e| vec![e])?;

    for global in mir_prog.globals() {
        //println!("GLOBAL: {:#?}", global);
    }

    let prog = mir_prog.compile().map_err(|e| vec![e])?;

    println!("{:?}", prog);

    Ok(vm::Vm::default().execute(&prog))
}

pub fn run_expr(src: &str) -> Result<(ty::Type, vm::Value), Vec<Error>> {
    let tokens = lex::lex(&src)?;
    let ast = ast::parse_expr(&tokens)?;

    let mut hir_prog = hir::Program::new();
    hir_prog
        .insert_def(&ast::Def::main(ast))
        .map_err(|e| vec![e])?;

    // TODO: Get rid of this
    let main_ident = LocalIntern::new("main".to_string());

    let mir_prog = mir::Program::from_hir(&hir_prog, main_ident).map_err(|e| vec![e])?;
    let prog = mir_prog.compile().map_err(|e| vec![e])?;

    Ok((
        hir_prog
            .root()
            .def(main_ident)
            .unwrap()
            .body
            .ty()
            .inner()
            .clone(),
        vm::Vm::default().execute(&prog),
    ))
}
