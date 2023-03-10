mod error;

pub use tao_middle::OptMode;
pub use tao_syntax::SrcId;

use ariadne::sources;
use error::Error;
use internment::Intern;
use std::{collections::HashMap, fmt, io::Write, str::FromStr};
use structopt::StructOpt;
use tao_analysis::Context as HirContext;
use tao_middle::Context;
use tao_syntax::{ast, parse_module, Error as SyntaxError, SrcNode};
use tao_vm::Program;

#[derive(Clone, Debug, Default, StructOpt)]
pub struct Options {
    /// Add a debugging layer to stdout (ast, hir, call_graph, mir, bytecode)
    #[structopt(long)]
    pub debug: Vec<String>,
    /// Specify an optimisation mode (none, fast, size)
    #[structopt(short, long, default_value = "none")]
    pub opt: OptMode,
}

pub fn compile<F: FnMut(SrcId) -> Option<String>, G: FnMut(SrcId, &str) -> Option<SrcId>>(
    src: String,
    src_id: SrcId,
    options: Options,
    mut writer: impl Write,
    mut get_file: F,
    mut make_src: G,
) -> Option<Program> {
    let (mut ast, mut syntax_errors) = parse_module(&src, src_id);

    // TODO: Write a proper module system you lazy git
    fn resolve_imports<
        F: FnMut(SrcId) -> Option<String>,
        G: FnMut(SrcId, &str) -> Option<SrcId>,
    >(
        parent_src: SrcId,
        module: Option<&mut ast::Module>,
        imported: &mut HashMap<SrcId, String>,
        import_errors: &mut Vec<Error>,
        syntax_errors: &mut Vec<SyntaxError>,
        get_file: &mut F,
        make_src: &mut G,
    ) {
        if let Some(module) = module {
            let imports = std::mem::take(&mut module.imports);

            for import in imports {
                match make_src(parent_src, import.as_str())
                    .and_then(|src_id| Some((src_id, get_file(src_id)?)))
                {
                    Some((src_id, src)) => {
                        // Check for cycles
                        if imported.insert(src_id, src.clone()).is_none() {
                            let (mut ast, mut new_syntax_errors) = parse_module(&src, src_id);
                            syntax_errors.append(&mut new_syntax_errors);

                            resolve_imports(
                                src_id,
                                ast.as_deref_mut(),
                                imported,
                                import_errors,
                                syntax_errors,
                                get_file,
                                make_src,
                            );

                            if let Some(mut ast) = ast {
                                let mut old_items = std::mem::take(&mut module.items);
                                module.items.append(&mut ast.items);
                                module.items.append(&mut old_items);
                            }
                        }
                    }
                    None => import_errors.push(Error::CannotImport(import.clone())),
                }
            }
        }
    }

    // Resolve imports
    let mut imported = HashMap::new();
    let mut import_errors = Vec::new();
    resolve_imports(
        src_id,
        ast.as_deref_mut(),
        &mut imported,
        &mut import_errors,
        &mut syntax_errors,
        &mut get_file,
        &mut make_src,
    );
    let mut srcs = sources(imported.into_iter().chain(std::iter::once((src_id, src))));
    if !import_errors.is_empty() {
        for e in import_errors {
            e.write(&mut srcs, &mut writer);
        }
        return None;
    }

    let mut syntax_error = false;
    for e in syntax_errors {
        syntax_error = true;
        e.write(&mut srcs, &mut writer);
    }

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

        #[cfg(feature = "debug")]
        if options.debug.contains(&"call_graph".to_string()) {
            ctx.generate_call_graph().render_to(&mut writer).unwrap();
        }

        if !analysis_errors.is_empty() || syntax_error {
            for e in analysis_errors {
                e.write(&ctx, &mut srcs, src_id, &mut writer);
            }
            None
        } else {
            let (concrete, con_errors) = ctx.concretize();

            if !con_errors.is_empty() {
                for e in con_errors {
                    e.write(&ctx, &mut srcs, src_id, &mut writer);
                }
                None
            } else {
                let mut ctx = Context::from_concrete(&ctx, &concrete);

                ctx.optimize(options.opt);

                if options.debug.contains(&"mir".to_string()) {
                    for (id, proc) in ctx.procs.iter() {
                        writeln!(writer, "PROCEDURE {:?}\n\n{}\n", id, proc.body.print()).unwrap();
                    }
                }

                let prog = Program::from_mir(&ctx);

                if options.debug.contains(&"bytecode".to_string()) {
                    prog.write(&mut writer);
                }

                Some(prog)
            }
        }
    } else {
        None
    }
}
