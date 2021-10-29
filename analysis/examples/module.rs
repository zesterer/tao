use tao_syntax::{SrcId, parse_module};
use tao_analysis::{ToHir, Scope, Reify, Context, Infer};
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

    // println!("{:?}", ast);

    if let Some(ast) = ast {
        let (ctx, errors) = Context::from_module(&ast);

        for err in errors {
            err.print(&ctx, sources([(src_id, &src)]));
        }
    }
}
