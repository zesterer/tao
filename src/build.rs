use super::*;

pub type PkgId = Ident;

#[derive(Debug, Default)]
pub struct Build {
    is_err: bool,
    pkgs: HashMap<PkgId, hir::Pkg>,
}

pub fn lower_pkg(build: &RwLock<Build>, id: PkgId, pkg: &SrcNode<syntax::Module>) {
    let (is_err, pkg) = {
        let build = &*build.read().unwrap();
        let mut pkg_ctx = hir::PkgCtx::new(build);
        pkg_ctx.lower_module(pkg);
        (pkg_ctx.is_err, pkg_ctx.pkg)
    };
    let mut build = build.write().unwrap();
    build.is_err |= is_err;
    build.pkgs.insert(id, pkg);
}
