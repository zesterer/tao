mod lex;
mod parse;
mod eval;

use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();

    loop {
        let line = rl.readline(">> ");
        match line {
            Ok(line) => {
                let tokens = lex::lex(&line);
                println!("Tokens: {:?}", tokens);
                let ast = parse::parse(&tokens.unwrap());
                println!("AST: {:?}", ast);
                let result = eval::eval(&ast.unwrap());
                println!("Result: {:?}", result);
            },
            Err(_) => break,
        }
    }
}
