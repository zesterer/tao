use super::*;

impl Context {
    pub fn generate_call_graph(&self) -> CallGraph<'_> {
        CallGraph { ctx: self }
    }
}

pub struct CallGraph<'a> {
    ctx: &'a Context,
}

impl<'a> CallGraph<'a> {
    pub fn render_to(&self, mut w: impl std::io::Write) -> std::io::Result<()> {
        dot::render(self, &mut w)
    }
}

type Node = (DefId, Ident);
type Edge = (Node, Node);

impl<'hir, 'a> dot::Labeller<'a, Node, Edge> for CallGraph<'hir> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("Program").unwrap()
    }

    fn node_id(&'a self, (id, _): &Node) -> dot::Id<'a> {
        dot::Id::new(format!("def_{}", id.0)).unwrap()
    }
    fn node_label<'b>(&'b self, (_, name): &Node) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr(name.to_string().into())
    }
    fn edge_label<'b>(&'b self, _: &Edge) -> dot::LabelText<'b> {
        dot::LabelText::LabelStr("".into())
    }
}

impl<'hir, 'a> dot::GraphWalk<'a, Node, Edge> for CallGraph<'hir> {
    fn nodes(&'a self) -> dot::Nodes<'a, Node> {
        self.ctx
            .defs
            .iter()
            .filter_map(|(id, def)| {
                if def.attr.iter().find(|a| &**a.name == "util").is_none() {
                    Some((id, *def.name))
                } else {
                    None
                }
            })
            .collect()
    }
    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        fn fetch_edge(ctx: &Context, parent: Node, expr: &TyExpr, edges: &mut HashSet<Edge>) {
            if let hir::Expr::Global(global) = &**expr {
                let def = ctx.defs.get(global.0);
                if def.attr.iter().find(|a| &**a.name == "util").is_none() {
                    edges.insert((parent, (global.0, *def.name)));
                }
            }

            expr.for_children(|e| fetch_edge(ctx, parent, e, edges));
        }

        let mut edges = HashSet::new();
        for (id, def) in self.ctx.defs.iter() {
            if def.attr.iter().find(|a| &**a.name == "util").is_none() {
                let parent = (id, *def.name);
                fetch_edge(self.ctx, parent, def.body.as_ref().unwrap(), &mut edges);
            }
        }
        edges.into_iter().collect()
    }
    fn source(&self, e: &Edge) -> Node {
        e.0
    }
    fn target(&self, e: &Edge) -> Node {
        e.1
    }
}
