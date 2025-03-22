pub mod build;
pub mod error;
pub mod hir;
pub mod syntax;
pub mod util;

use crate::{
    build::{lower_pkg, Build, PkgId},
    error::Error,
    syntax::{lexer, parsers, Filename, Ident, Span},
    util::SrcNode,
};
use chumsky::{input::Input as _, span::Span as _, Parser as _};
use std::{
    collections::{hash_map::Entry, HashMap},
    path::PathBuf,
    sync::RwLock,
};

use internment::ArcIntern;

fn main() {
    let build = RwLock::new(Build::default());

    let path = Filename(ArcIntern::new(PathBuf::from("examples/bootstrap.tao")));
    let src = std::fs::read_to_string(&*path.0).unwrap();
    let eoi = Span::new(path.clone(), 0..src.len());
    let tokens = dbg!(lexer().parse(src.with_context(path))).unwrap();
    let module = dbg!(parsers()
        .module
        .parse(tokens.map(eoi, |tt| (&tt.inner, &tt.span))))
    .unwrap();

    lower_pkg(&build, PkgId::from_ref("root"), &module);

    dbg!(&*build.read().unwrap());
}
