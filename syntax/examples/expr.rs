use rustyline::Editor;
use tao_syntax::{SrcId, parse_expr};
use ariadne::sources;

fn main() {
    let mut rl = Editor::<()>::new();

    loop {
        let line = rl.readline("\n>> ");
        match line {
            Ok(line) => {
                rl.add_history_entry(&line);

                let (ast, errs) = parse_expr(&line, SrcId::repl());

                for err in errs {
                    err.print(sources([(SrcId::repl(), &line)]));
                }

                println!("{:?}", ast);
            }
            Err(_) => break,
        }
    }
}
