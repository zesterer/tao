#![type_length_limit="10823821"]
#![feature(arbitrary_self_types)]

mod lex;
mod parse;
mod eval;
mod src;
mod node;
mod error;
mod hir;
mod compile;
mod ast;
mod ty;
mod hir2;
mod mir;
mod node2;
mod scope;
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

    // --------------- NEW STUFF BEGIN ---------------

    let mut ast2 = match ast::parse_module(&tokens) {
        Ok(ast2) => ast2,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(src));
            }
            return;
        },
    };
    println!("AST2: {:#?}", ast2);

    let hir_prog = match hir2::Program::new_root(&ast2) {
        Ok(hir_prog) => hir_prog,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    let main_ident = LocalIntern::new("main".to_string());
    println!("TYPE2: {}", **hir_prog.root().def(main_ident).unwrap().body.ty());

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

    // -------------- NEW STUFF END ---------------

    /*
    let mut module = match parse::parse_module(&tokens) {
        Ok(module) => module,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(src));
            }
            return;
        },
    };

    if let Err(err) = module.ascribe_types() {
        //println!("Module: {:#?}", module);
        print!("{}", err.in_source(src));
        return;
    }

    let program = match compile::Program::from_module(&module) {
        Ok(program) => program,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    //println!("Program: {}", program);

    let result = match eval::Vm::default().execute(&program) {
        Ok(result) => result,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    println!("{}", result);
    */
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

    // --------------- NEW STUFF BEGIN ---------------

    let mut ast2 = match ast::parse_expr(&tokens) {
        Ok(ast2) => ast2,
        Err(errs) => {
            for err in errs {
                print!("AST2: {}", err.in_source(src));
            }
            return;
        },
    };
    println!("AST2: {:#?}", ast2);

    let mut prog = hir2::Program::new();

    match prog.insert_def(&[], &ast::Def::main(ast2)) {
        Ok(()) => {},
        Err(err) => {
            print!("AST2: {}", err.in_source(src));
            return;
        },
    };

    let main_ident = LocalIntern::new("main".to_string());
    println!("TYPE2: {:?}", prog.root().def(main_ident).unwrap().body.ty());

    // -------------- NEW STUFF END ---------------

    let mut ast = match parse::parse_expr(&tokens) {
        Ok(ast) => ast,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(src));
            }
            return;
        },
    };

    if let Err(err) = ast.ascribe_types() {
        //println!("AST: {:#?}", ast);
        print!("{}", err.in_source(src));
        return;
    }

    println!("{}", ast.meta.inner);

    let program = match compile::Program::from_expr(&ast) {
        Ok(program) => program,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    //println!("Program: {:?}", program);

    let result = match eval::Vm::default().execute(&program) {
        Ok(result) => result,
        Err(err) => {
            print!("{}", err.in_source(src));
            return;
        },
    };

    println!("{}", result);
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
