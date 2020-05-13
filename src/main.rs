#![type_length_limit="10823821"]
#![feature(arbitrary_self_types)]

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

fn run_module(src: &str) {
    let tokens = match lex::lex(&src) {
        Ok(tokens) => tokens,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(src));
            }
            return;
        },
    };

    let mut ast = match ast::parse_module(&tokens) {
        Ok(ast) => ast,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(src));
            }
            return;
        },
    };
    println!("AST: {:#?}", ast);

    let hir_prog = match hir::Program::new_root(&ast) {
        Ok(hir_prog) => hir_prog,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    let main_ident = LocalIntern::new("main".to_string());
    println!("TYPE: {}", **hir_prog.root().def(main_ident).unwrap().body.ty());

    let mir_prog = match mir::Program::from_hir(&hir_prog, main_ident) {
        Ok(mir_prog) => mir_prog,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    let prog = match mir_prog.compile() {
        Ok(prog) => prog,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    let output = vm::Vm::default()
        .execute(&prog);

    println!("Output: {:?}", output);
}

fn run_expr(src: &str) {
    let tokens = match lex::lex(&src) {
        Ok(tokens) => tokens,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(src));
            }
            return;
        },
    };

    let mut ast = match ast::parse_expr(&tokens) {
        Ok(ast) => ast,
        Err(errs) => {
            for err in errs {
                print!("AST: {}", err.in_source(src));
            }
            return;
        },
    };
    println!("AST: {:#?}", ast);

    let mut hir_prog = hir::Program::new();

    match hir_prog.insert_def(&[], &ast::Def::main(ast)) {
        Ok(()) => {},
        Err(err) => {
            print!("AST: {}", err.in_source(src));
            return;
        },
    };

    let main_ident = LocalIntern::new("main".to_string());
    println!("TYPE: {:?}", hir_prog.root().def(main_ident).unwrap().body.ty());

    let mir_prog = match mir::Program::from_hir(&hir_prog, main_ident) {
        Ok(mir_prog) => mir_prog,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    let prog = match mir_prog.compile() {
        Ok(prog) => prog,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    let output = vm::Vm::default()
        .execute(&prog);

    println!("Output: {:?}", output);
}

fn main() {
    if let Some(filename) = env::args().nth(1) {
        let mut buf = String::new();
        File::open(&filename)
            .and_then(|mut file| file.read_to_string(&mut buf))
            .unwrap_or_else(|err| panic!("Could not read file '{}': {:?}", filename, err));

        run_module(&buf);
    } else {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline(">> ");
            match line {
                Ok(line) => {
                    rl.add_history_entry(&line);
                    run_expr(&line);
                },
                Err(_) => break,
            }
        }
    }
}
