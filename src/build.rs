use super::*;

pub type PkgId = Ident;

#[derive(Debug, Default)]
pub struct Build {
    pub is_err: bool,
    pkgs: HashMap<PkgId, hir::Pkg>,
}

pub fn lower_pkg(build: &RwLock<Build>, id: PkgId, fname: Filename) {
    let (is_err, pkg) = {
        let build = &*build.read().unwrap();
        let mut pkg_ctx = hir::PkgCtx::new(build);

        // Read from disk
        let src = std::fs::read_to_string(&*fname.0).unwrap();
        let eoi = Span::new(fname.clone(), 0..src.len());

        // Lex
        let (tokens, errors) = lexer().parse(src.with_context(fname)).into_output_errors();
        for err in errors {
            pkg_ctx.emit_error(err.into());
        }
        let tokens = tokens.unwrap_or_default();

        // Parser
        let (module, errors) = parsers()
            .module
            .parse(tokens.map(eoi, |tt| (&tt.inner, &tt.span)))
            .into_output_errors();
        for err in errors {
            pkg_ctx.emit_error(err.into());
        }

        // Lower to HIR
        if let Some(module) = module {
            pkg_ctx.declare_module(hir::ItemPath::default(), &module);
            pkg_ctx.lower_module(hir::ItemPath::default(), &module);
        }

        (pkg_ctx.is_err, pkg_ctx.pkg)
    };
    let mut build = build.write().unwrap();
    build.is_err |= is_err;
    build.pkgs.insert(id, pkg);
}
