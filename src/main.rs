#![type_length_limit="10823821"]

mod lex;
mod parse;
mod eval;
mod src;
mod node;
mod error;
mod compile;

use std::{
    env,
    fs::File,
    io::Read,
};
use rustyline::Editor;

fn run(expr: &str) {
    let tokens = match lex::lex(&expr) {
        Ok(tokens) => tokens,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(expr));
            }
            return;
        },
    };

    //println!("TOKENS: {:#?}", tokens);

    let ast = match parse::parse(&tokens) {
        Ok(ast) => ast,
        Err(errs) => {
            for err in errs {
                print!("{}", err.in_source(expr));
            }
            return;
        },
    };

    //println!("AST: {:#?}", ast);

    let program = match compile::Program::compile(&ast) {
        Ok(program) => program,
        Err(err) => {
            print!("{}", err.in_source(expr));
            return;
        },
    };

    //println!("Program: {:?}", program);

    let result = match eval::Vm::default().execute(&program) {
        Ok(result) => result,
        Err(err) => {
            print!("{}", err.in_source(expr));
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

        run(&buf);
    } else {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline(">> ");
            match line {
                Ok(line) => run(&line),
                Err(_) => break,
            }
        }
    }
}
