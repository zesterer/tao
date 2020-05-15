#![type_length_limit="10823821"]
#![feature(arbitrary_self_types, arbitrary_enum_discriminant)]

mod lex;
mod src;
mod node;
mod error;
mod ast;
mod ty;
mod hir;
mod mir;
mod vm;

use std::{
    env,
    fs::File,
    io::Read,
};
use internment::LocalIntern;
use rustyline::Editor;
use crate::error::Error;

fn run_module(src: &str) -> Result<vm::Value, Vec<Error>> {
    let tokens = lex::lex(&src)?;
    let ast = ast::parse_module(&tokens)?;
    let hir_prog = hir::Program::new_root(&ast).map_err(|e| vec![e])?;

    // TODO: Get rid of this
    let main_ident = LocalIntern::new("main".to_string());
    if let Some(main) = hir_prog.root().def(main_ident) {
        println!("TYPE: {}", **main.body.ty());
    }

    let mir_prog = mir::Program::from_hir(&hir_prog, main_ident).map_err(|e| vec![e])?;
    let prog = mir_prog.compile().map_err(|e| vec![e])?;

    println!("{:?}", prog);

    Ok(vm::Vm::default().execute(&prog))
}

fn run_expr(src: &str) -> Result<vm::Value, Vec<Error>> {
    let tokens = lex::lex(&src)?;
    let ast = ast::parse_expr(&tokens)?;

    let mut hir_prog = hir::Program::new();
    hir_prog.insert_def(&Default::default(), &ast::Def::main(ast)).map_err(|e| vec![e])?;

    // TODO: Get rid of this
    let main_ident = LocalIntern::new("main".to_string());
    println!("TYPE: {}", **hir_prog.root().def(main_ident).unwrap().body.ty());

    let mir_prog = mir::Program::from_hir(&hir_prog, main_ident).map_err(|e| vec![e])?;
    let prog = mir_prog.compile().map_err(|e| vec![e])?;

    Ok(vm::Vm::default().execute(&prog))
}

fn main() {
    if let Some(filename) = env::args().nth(1) {
        let mut src = String::new();
        File::open(&filename)
            .and_then(|mut file| file.read_to_string(&mut src))
            .unwrap_or_else(|err| panic!("Could not read file '{}': {:?}", filename, err));

        match run_module(&src) {
            Ok(val) => println!("{}", val),
            Err(errs) => errs.iter().for_each(|err| print!("{}", err.in_source(&src))),
        };
    } else {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline(">> ");
            match line {
                Ok(line) => {
                    rl.add_history_entry(&line);

                    match run_expr(&line) {
                        Ok(val) => println!("{}", val),
                        Err(errs) => errs.iter().for_each(|err| print!("{}", err.in_source(&line))),
                    };
                },
                Err(_) => break,
            }
        }
    }
}
