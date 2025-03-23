use super::*;

pub type PkgId = Filename;

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
        let src = match std::fs::read_to_string(&*fname.0) {
            Ok(src) => src,
            Err(err) => panic!("Could not open `{fname}`: {err}"),
        };
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

#[derive(Clone, Debug)]
pub struct PkgManifest {
    pub name: String,
    pub root: PathBuf,
    pub deps: HashMap<String, (PkgId, Span)>,
}

fn parse_manifest(pkg_id: PkgId) -> Result<(PkgManifest, Span), Error> {
    use kdl::{KdlDocument, KdlNode};
    use miette::SourceSpan;

    let mut fname = (*pkg_id.0).clone();
    fname.push("pkg.kdl");
    let fname = Filename(ArcIntern::new(fname));
    let src = std::fs::read_to_string(&*fname.0).unwrap();

    let pkg = KdlDocument::parse_v2(&src).unwrap();

    fn make_span(fname: Filename, span: SourceSpan) -> Span {
        Span::new(fname, span.offset()..span.offset() + span.len())
    }

    fn get_key<'a>(doc: &'a KdlDocument, fname: Filename, key: &str) -> Result<&'a KdlNode, Error> {
        doc.get(key).ok_or_else(move || {
            Error::missing_manifest_key(Ident::from_ref(key), make_span(fname, doc.span()))
        })
    }

    let manifest = PkgManifest {
        name: get_key(&pkg, fname.clone(), "name")?
            .get(0)
            .unwrap()
            .as_string()
            .unwrap()
            .to_string(),
        root: get_key(&pkg, fname.clone(), "root")?
            .get(0)
            .unwrap()
            .as_string()
            .unwrap()
            .parse()
            .unwrap(),
        deps: get_key(&pkg, fname.clone(), "dependencies")?
            .iter_children()
            .map(|dep| {
                (
                    dep.name().value().to_string(),
                    (
                        Filename(ArcIntern::new(
                            dep.get(0).unwrap().as_string().unwrap().parse().unwrap(),
                        )),
                        make_span(fname.clone(), dep.span()),
                    ),
                )
            })
            .collect(),
    };

    Ok((manifest, Span::new(fname, 0..src.len())))
}

pub fn walk_deps(pkg_id: PkgId) -> Result<HashMap<PkgId, PkgManifest>, Error> {
    fn walk(
        pkg_id: PkgId,
        stack: &mut Vec<PkgId>,
        pkgs: &mut HashMap<PkgId, PkgManifest>,
    ) -> Result<(), Error> {
        if let Entry::Vacant(e) = pkgs.entry(pkg_id.clone()) {
            let (manifest, manifest_span) = parse_manifest(pkg_id.clone())?;

            e.insert(manifest.clone());

            stack.push(pkg_id);
            for (_, (dep_id, dep_span)) in &manifest.deps {
                if stack.contains(dep_id) {
                    stack.push(dep_id.clone());
                    return Err(Error::cyclic_dependency(
                        dep_id.clone(),
                        manifest_span,
                        stack.clone(),
                    ));
                } else {
                    walk(dep_id.clone(), stack, pkgs)?;
                }
            }
            stack.pop();
        }
        Ok(())
    }

    let mut pkgs = HashMap::default();
    walk(pkg_id, &mut Vec::new(), &mut pkgs)?;
    Ok(pkgs)
}
