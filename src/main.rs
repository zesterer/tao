pub mod build;
pub mod error;
pub mod hir;
pub mod mir;
pub mod syntax;
pub mod util;

use crate::{
    build::{Build, PkgId},
    error::Error,
    syntax::{lexer, parsers, Filename, Ident, Span},
    util::{Id, SrcNode, Store},
};
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use std::{
    collections::{hash_map::Entry, HashMap},
    path::PathBuf,
    process::ExitCode,
    sync::RwLock,
};

use internment::ArcIntern;

#[derive(clap::Parser)]
pub struct Args {
    path: Option<PathBuf>,
}

fn main() -> ExitCode {
    let args = <Args as clap::Parser>::parse();

    let root_pkg_id = Filename(ArcIntern::new(
        args.path
            .unwrap_or_else(|| std::env::current_dir().unwrap()),
    ));
    let mut build = match build::walk_deps(root_pkg_id.clone()) {
        Ok(build) => build,
        Err(err) => {
            err.emit();
            return ExitCode::FAILURE;
        }
    };

    std::thread::scope(|s| {
        let mut todo = build.pkgs.keys().cloned().collect::<Vec<_>>();
        let mut started = 0;
        let mut done = Vec::new();

        let (tx, rx) = std::sync::mpsc::channel::<PkgId>();

        loop {
            if todo.is_empty() {
                break;
            }

            // Start compiling any packages if their dependencies have all been compiled.
            for pkg_id in todo.extract_if(.., |todo| {
                build.pkgs[todo]
                    .0
                    .deps
                    .values()
                    .all(|(dep_id, _)| done.contains(dep_id))
            }) {
                started += 1;
                let pkg = &build.pkgs[&pkg_id].0;
                let root_module = Filename(ArcIntern::new(
                    [pkg_id.0.as_path(), pkg.root.as_path()]
                        .into_iter()
                        .collect(),
                ));
                s.spawn({
                    let build = &build;
                    let tx = tx.clone();
                    move || {
                        println!("[{started}/{}] Compiling {}...", build.pkgs.len(), pkg.name);
                        build::lower_pkg(build, pkg_id.clone(), root_module);
                        tx.send(pkg_id).unwrap();
                    }
                });
            }

            // When a package has finished compiling, add it to the done set
            done.push(rx.recv().unwrap());
        }
    });

    let mut prog = mir::Program::default();

    let root_pkg = build.pkgs[&root_pkg_id].1.get().unwrap();
    let entry_path = hir::ItemPath(vec![Ident::from_ref("main")]);
    let main_def = root_pkg.defs.lookup(&entry_path);

    if main_def.is_none() {
        Error::no_main(root_pkg.root_module_span.clone()).emit();
        *build.is_err.get_mut() = true;
    };

    // Final bail point of no return
    // If an error was previously encountered, we exit. From now on, all compiler errors are ICEs.
    if *build.is_err.get_mut() {
        println!(
            "Compilation of `{}` terminated due to previous errors.",
            build.pkgs[&root_pkg_id].0.name
        );
        return ExitCode::FAILURE;
    }

    prog.lower(&build, (root_pkg_id.clone(), main_def.unwrap()));

    println!(
        "Compilation of `{}` succeeded.",
        build.pkgs[&root_pkg_id].0.name
    );
    ExitCode::SUCCESS
}
