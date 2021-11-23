pub use tao_syntax::SrcId;

use tao_syntax::parse_module;
use tao_analysis::Context as HirContext;
use tao_middle::Context;
use tao_vm::{Program, exec};
use ariadne::sources;
use structopt::StructOpt;
use std::{
    str::FromStr,
    io::Write,
    fmt,
};

#[derive(Copy, Clone, Debug)]
pub enum Opt {
    None,
    Fast,
    Size,
}

impl FromStr for Opt {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s {
            "none" => Ok(Opt::None),
            "fast" => Ok(Opt::Fast),
            "size" => Ok(Opt::Size),
            _ => Err("Optimisation mode does not exist"),
        }
    }
}

impl fmt::Display for Opt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opt::None => write!(f, "none"),
            Opt::Fast => write!(f, "fast"),
            Opt::Size => write!(f, "size"),
        }
    }
}

#[derive(Clone, Debug, StructOpt)]
pub struct Options {
    /// Add a debugging layer to stdout (tokens, ast, hir, mir, bytecode)
    #[structopt(long)]
    pub debug: Vec<String>,
    /// Specify an optimisation mode (none, fast, size)
    #[structopt(short, long, default_value = "none")]
    pub opt: Opt,
}

pub fn run(src: String, src_id: SrcId, options: Options, mut writer: impl Write) {
    let (ast, syntax_errors) = parse_module(&src, src_id);

    let mut syntax_error = false;
    for e in syntax_errors {
        syntax_error = true;
        e.write(sources([(src_id, &src)]), &mut writer);
    }

    // println!("Items = {}", ast.as_ref().unwrap().items.len());
    if options.debug.contains(&"ast".to_string()) {
        writeln!(writer, "{:?}", ast).unwrap();
    }

    if let Some(ast) = ast {
        let (ctx, analysis_errors) = HirContext::from_module(&ast);

        if options.debug.contains(&"hir".to_string()) {
            for (_, def) in ctx.defs.iter() {
                writeln!(writer, "{} = {:?}", *def.name, def.body).unwrap();
            }
        }

        if !analysis_errors.is_empty() || syntax_error {
            for e in analysis_errors {
                e.write(&ctx, sources([(src_id, &src)]), &mut writer);
            }
        } else {
            let (mut ctx, errors) = Context::from_hir(&ctx);

            for err in errors {
                err.write(&ctx, sources([(src_id, &src)]), &mut writer);
            }

            match options.opt {
                Opt::None => {},
                Opt::Fast => ctx.optimize(),
                Opt::Size => todo!("Implement size optimization"),
            }

            if options.debug.contains(&"mir".to_string()) {
                writeln!(writer, "{:?}", ctx.procs.get(ctx.entry.unwrap()).unwrap().body).unwrap();
            }

            let prog = Program::from_mir(&ctx);

            if options.debug.contains(&"bytecode".to_string()) {
                prog.write(&mut writer);
            }

            if let Some(result) = exec(&prog) {
                writeln!(writer, "{}", result).unwrap();
            }
        }
    }
}
