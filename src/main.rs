pub mod build;
pub mod error;
pub mod hir;
pub mod syntax;
pub mod util;

use crate::{
    build::{lower_pkg, Build, PkgId},
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

fn main() {
    let build = RwLock::new(Build::default());

    lower_pkg(
        &build,
        PkgId::from_ref("root"),
        Filename(ArcIntern::new(PathBuf::from("examples/bootstrap.tao"))),
    );

    let build = build.into_inner().unwrap();

    if !build.is_err {
        dbg!(build);
    }
}
