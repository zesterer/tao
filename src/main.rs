pub mod build;
pub mod error;
pub mod hir;
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
    sync::RwLock,
};

use internment::ArcIntern;

fn parse_dep(s: &str) -> Result<(String, PathBuf), String> {
    use chumsky::prelude::*;
    let dep = text::ident::<_, extra::Err<Rich<char>>>()
        .then_ignore(just('@'))
        .then(any().repeated().to_slice());
    dep.parse(s)
        .into_result()
        .map(|(name, path)| (name.to_string(), PathBuf::from(path)))
        .map_err(|mut errs| errs.remove(0).to_string())
}

#[derive(clap::Parser)]
pub struct Args {
    path: Option<PathBuf>,
    // #[arg(value_parser = parse_dep)]
    // pkg: (String, PathBuf),
    // #[arg(long = "dep", value_parser = parse_dep)]
    // deps: Vec<(String, PathBuf)>,
}

fn main() {
    let args = <Args as clap::Parser>::parse();

    let manifest_path = Filename(ArcIntern::new(
        args.path
            .unwrap_or_else(|| std::env::current_dir().unwrap()),
    ));
    let pkgs = match build::walk_deps(manifest_path) {
        Ok(pkgs) => pkgs,
        Err(err) => {
            err.emit();
            return;
        }
    };

    let build = RwLock::new(Build::default());

    std::thread::scope(|s| {
        let mut todo = pkgs.keys().cloned().collect::<Vec<_>>();
        let mut done = Vec::new();

        let (tx, rx) = std::sync::mpsc::channel::<PkgId>();

        loop {
            if todo.is_empty() {
                break;
            }

            // Start compiling any packages if their dependencies have all been compiled.
            for pkg_id in todo.extract_if(.., |todo| {
                pkgs[todo]
                    .deps
                    .values()
                    .all(|(dep_id, _)| done.contains(dep_id))
            }) {
                let pkg = &pkgs[&pkg_id];
                let root_module = Filename(ArcIntern::new(
                    [pkg_id.0.as_path(), pkg.root.as_path()]
                        .into_iter()
                        .collect(),
                ));
                s.spawn({
                    let build = &build;
                    let tx = tx.clone();
                    move || {
                        println!("Compiling {}...", pkg.name);
                        build::lower_pkg(build, pkg_id.clone(), root_module);
                        tx.send(pkg_id).unwrap();
                    }
                });
            }

            // When a package has finished compiling, add it to the done set
            done.push(rx.recv().unwrap());
        }
    });

    let build = build.into_inner().unwrap();

    // if !build.is_err {
    //     dbg!(build);
    // }
}
