use tao_syntax::{SrcId, parse_module};
use tao_analysis::Context as HirContext;
use tao_middle::Context;
use tao_vm::{Program, exec};
use ariadne::sources;
use std::{env, fs};

fn main() {
    let fname = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&fname).expect("Failed to read file");
    let src_id = SrcId::from_path(fname);

    let (ast, errs) = parse_module(&src, src_id);

    for err in errs {
        err.print(sources([(src_id, &src)]));
    }

    // println!("Items = {}", ast.as_ref().unwrap().items.len());
    // println!("{:?}", ast);

    if let Some(ast) = ast {
        let (ctx, errors) = HirContext::from_module(&ast);

        if !errors.is_empty() {
            for err in errors {
                err.print(&ctx, sources([(src_id, &src)]));
            }
        } else {
            let (mut ctx, errors) = Context::from_hir(&ctx);

            for err in errors {
                err.print(&ctx, sources([(src_id, &src)]));
            }

            ctx.optimize();

            println!("MIR: {:?}", ctx.procs.get(ctx.entry.unwrap()).unwrap().body);

            let prog = Program::from_mir(&ctx);

            prog.display();

            if let Some(result) = exec(&prog) {
                println!("Result: {}", result);
            }
        }
    }
}
