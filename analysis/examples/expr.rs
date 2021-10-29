use rustyline::Editor;
use tao_syntax::{SrcId, parse_expr};
use tao_analysis::{ToHir, Scope, Reify, Context, Infer};
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

                if let Some(ast) = ast {
                    let mut ctx = Context::default();
                    let mut infer = Infer::new(&mut ctx);

                    let hir = ast.to_hir(&mut infer, &Scope::Empty);

                    println!("{:?}", hir);

                    let (mut checked, errs) = infer.into_checked();

                    for err in errs {
                        println!("{:?}", err);
                        // err.print(sources([(SrcId::repl(), &line)]));
                    }

                    let hir_checked = hir.reify(&mut checked);

                    println!("Type = {}", ctx.tys.display(hir_checked.meta().1));
                }
            }
            Err(_) => break,
        }
    }
}
