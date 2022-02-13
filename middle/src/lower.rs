use super::*;

fn prim_to_mir(prim: ty::Prim) -> repr::Prim {
    match prim {
        ty::Prim::Nat => repr::Prim::Nat,
        ty::Prim::Int => repr::Prim::Int,
        ty::Prim::Char => repr::Prim::Char,
        ty::Prim::Bool => repr::Prim::Bool,
        p => todo!("{:?}", p),
    }
}

// Type instantiations for generic types
pub struct TyInsts<'a> {
    pub self_ty: Option<Repr>,
    pub gen: &'a [Repr],
}

impl Context {
    pub fn lower_def(&mut self, hir: &HirContext, con: &ConContext, def: ConDefId) -> ProcId {
        let id = self.procs.id_of_con(def.clone());

        // Instantiate proc if not already done
        if !self.procs.is_declared(id) {
            self.procs.declare(id);
            let proc = Proc {
                body: self.lower_expr(hir, con, con.get_def(def)),
            };
            self.procs.define(id, proc);
        }

        id
    }

    pub fn lower_litr(&mut self, hir: &HirContext, con: &ConContext, litr: &hir::Literal) -> Const {
        match litr {
            hir::Literal::Nat(x) => mir::Const::Nat(*x),
            hir::Literal::Str(s) => mir::Const::Str(*s),
            hir::Literal::Bool(x) => mir::Const::Bool(*x),
            hir::Literal::Real(x) => mir::Const::Real(*x),
            hir::Literal::Char(c) => mir::Const::Char(*c),
        }
    }

    pub fn lower_ty(&mut self, hir: &HirContext, con: &ConContext, ty: ConTyId) -> Repr {
        fn prim_to_mir(prim: ty::Prim) -> repr::Prim {
            match prim {
                ty::Prim::Nat => repr::Prim::Nat,
                ty::Prim::Int => repr::Prim::Int,
                ty::Prim::Real => repr::Prim::Real,
                ty::Prim::Char => repr::Prim::Char,
                ty::Prim::Bool => repr::Prim::Bool,
            }
        }

        match con.get_ty(ty) {
            ConTy::Prim(prim) => Repr::Prim(prim_to_mir(*prim)),
            ConTy::List(item) => Repr::List(Box::new(self.lower_ty(hir, con, *item))),
            ConTy::Tuple(fields) => Repr::Tuple(fields
                .iter()
                .map(|field| self.lower_ty(hir, con, *field))
                .collect()),
            ConTy::Func(i, o) => Repr::Func(
                Box::new(self.lower_ty(hir, con, *i)),
                Box::new(self.lower_ty(hir, con, *o)),
            ),
            ConTy::Data(data_id) => {
                let args = data_id.1
                    .iter()
                    .map(|arg| self.lower_ty(hir, con, *arg))
                    .collect::<Vec<_>>();

                if self.reprs.declare(*data_id) {
                    let variants = con
                        .get_data(*data_id)
                        .cons
                        .iter()
                        .map(|(_, ty)| self.lower_ty(hir, con, *ty))
                        .collect();
                    self.reprs.define(*data_id, Repr::Sum(variants));
                }
                Repr::Data(*data_id)
            },
            ConTy::Record(fields) => {
                let mut fields = fields
                    .into_iter()
                    .map(|(name, field)| (*name, self.lower_ty(hir, con, *field)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                Repr::Tuple(fields.into_iter().map(|(_, ty)| ty).collect())
            },
        }
    }

    pub fn lower_binding(&mut self, hir: &HirContext, con: &ConContext, con_binding: &ConBinding) -> mir::MirNode<mir::Binding> {
        let pat = match &*con_binding.pat {
            hir::Pat::Error => unreachable!(),
            hir::Pat::Wildcard => mir::Pat::Wildcard,
            hir::Pat::Literal(litr) => mir::Pat::Const(self.lower_litr(hir, con, litr)),
            hir::Pat::Single(inner) => mir::Pat::Single(self.lower_binding(hir, con, inner)),
            hir::Pat::Add(lhs, rhs) => mir::Pat::Add(self.lower_binding(hir, con, lhs), **rhs),
            hir::Pat::Tuple(fields) => mir::Pat::Tuple(fields
                .iter()
                .map(|field| self.lower_binding(hir, con, field))
                .collect()),
            hir::Pat::ListExact(items) => mir::Pat::ListExact(items
                .iter()
                .map(|item| self.lower_binding(hir, con, item))
                .collect()),
            hir::Pat::ListFront(items, tail) => mir::Pat::ListFront(
                items
                    .iter()
                    .map(|item| self.lower_binding(hir, con, item))
                    .collect(),
                tail.as_ref().map(|tail| self.lower_binding(hir, con, tail)),
            ),
            hir::Pat::Decons(data, variant, inner) => {
                let variant = hir.datas
                    .get_data(**data)
                    .cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| **name == *variant)
                    .unwrap()
                    .0;
                mir::Pat::Variant(variant, self.lower_binding(hir, con, inner))
            },
            hir::Pat::Record(fields) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, field)| (*name, self.lower_binding(hir, con, field)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                mir::Pat::Tuple(fields.into_iter().map(|(_, field)| field).collect())
            },
            pat => todo!("{:?}", pat),
        };

        let binding = mir::Binding {
            pat,
            name: con_binding.name.as_ref().map(|n| **n),
        };

        MirNode::new(binding, self.lower_ty(hir, con, *con_binding.meta()))
    }

    pub fn lower_expr(&mut self, hir: &HirContext, con: &ConContext, con_expr: &ConExpr) -> mir::MirNode<mir::Expr> {
        let expr = match &**con_expr {
            hir::Expr::Error => unreachable!(),
            hir::Expr::Literal(litr) => mir::Expr::Const(self.lower_litr(hir, con, litr)),
            hir::Expr::Local(local) => mir::Expr::Local(*local),
            hir::Expr::Global(def_id, args) => {
                mir::Expr::Global(self.lower_def(hir, con, Intern::new((*def_id, args.clone()))), Default::default())
            },
            hir::Expr::Unary(op, x) => {
                use ast::UnaryOp::*;
                use ty::Prim::*;
                use ConTy::{Prim, List};
                let intrinsic = match (**op, con.get_ty(*x.meta())) {
                    (Not, Prim(Bool)) => mir::Intrinsic::NotBool,
                    (Neg, Prim(Nat)) => mir::Intrinsic::NegNat,
                    (Neg, Prim(Int)) => mir::Intrinsic::NegInt,
                    op => panic!("Invalid unary op in HIR: {:?}", op),
                };
                mir::Expr::Intrinsic(intrinsic, vec![self.lower_expr(hir, con, x)])
            },
            hir::Expr::Binary(op, x, y) => {
                use ast::BinaryOp::*;
                use ty::Prim::*;
                use ConTy::{Prim, List};
                let intrinsic = match (**op, con.get_ty(*x.meta()), con.get_ty(*y.meta())) {
                    (Add, Prim(Nat), Prim(Nat)) => mir::Intrinsic::AddNat,
                    (Add, Prim(Int), Prim(Int)) => mir::Intrinsic::AddInt,
                    (Sub, Prim(Nat), Prim(Nat)) => mir::Intrinsic::SubNat,
                    (Sub, Prim(Int), Prim(Int)) => mir::Intrinsic::SubInt,
                    (Mul, Prim(Nat), Prim(Nat)) => mir::Intrinsic::MulNat,
                    (Mul, Prim(Int), Prim(Int)) => mir::Intrinsic::MulInt,
                    (Eq, Prim(Nat), Prim(Nat)) => mir::Intrinsic::EqNat,
                    (Eq, Prim(Int), Prim(Int)) => mir::Intrinsic::EqInt,
                    (NotEq, Prim(Nat), Prim(Nat)) => mir::Intrinsic::NotEqNat,
                    (NotEq, Prim(Int), Prim(Int)) => mir::Intrinsic::NotEqInt,
                    (Less, Prim(Nat), Prim(Nat)) => mir::Intrinsic::LessNat,
                    (Less, Prim(Int), Prim(Int)) => mir::Intrinsic::LessInt,
                    (More, Prim(Nat), Prim(Nat)) => mir::Intrinsic::MoreNat,
                    (More, Prim(Int), Prim(Int)) => mir::Intrinsic::MoreInt,
                    (LessEq, Prim(Nat), Prim(Nat)) => mir::Intrinsic::LessEqNat,
                    (LessEq, Prim(Int), Prim(Int)) => mir::Intrinsic::LessEqInt,
                    (MoreEq, Prim(Nat), Prim(Nat)) => mir::Intrinsic::MoreEqNat,
                    (MoreEq, Prim(Int), Prim(Int)) => mir::Intrinsic::MoreEqInt,
                    (Eq, Prim(Char), Prim(Char)) => mir::Intrinsic::EqChar,
                    (NotEq, Prim(Char), Prim(Char)) => mir::Intrinsic::NotEqChar,
                    (Join, List(x), List(y)) => mir::Intrinsic::Join(self.lower_ty(hir, con, *x)), // Assume x = y
                    op => panic!("Invalid binary op in HIR: {:?}", op),
                };
                mir::Expr::Intrinsic(intrinsic, vec![self.lower_expr(hir, con, x), self.lower_expr(hir, con, y)])
            },
            hir::Expr::Match(_, pred, arms) => {
                let arms = arms
                    .iter()
                    .map(|(binding, body)| (self.lower_binding(hir, con, binding), self.lower_expr(hir, con, body)))
                    .collect();
                mir::Expr::Match(self.lower_expr(hir, con, pred), arms)
            },
            hir::Expr::Tuple(fields) => mir::Expr::Tuple(fields
                .iter()
                .map(|field| self.lower_expr(hir, con, field))
                .collect()),
            hir::Expr::List(items) => mir::Expr::List(items
                .iter()
                .map(|item| self.lower_expr(hir, con, item))
                .collect()),
            hir::Expr::ListFront(items, tail) => {
                let tail = self.lower_expr(hir, con, tail);
                mir::Expr::Intrinsic(
                    mir::Intrinsic::Join(match tail.meta() {
                        Repr::List(item) => (**item).clone(),
                        _ => unreachable!(),
                    }),
                    vec![
                        MirNode::new(mir::Expr::List(items
                            .iter()
                            .map(|item| self.lower_expr(hir, con, item))
                            .collect()), tail.meta().clone()),
                        tail,
                    ],
                )
            },
            hir::Expr::Func(arg, body) => {
                let body = self.lower_expr(hir, con, body);
                mir::Expr::Func(body.required_locals(Some(**arg)), **arg, body)
            },
            hir::Expr::Apply(f, arg) => mir::Expr::Apply(self.lower_expr(hir, con, f), self.lower_expr(hir, con, arg)),
            hir::Expr::Cons(data, variant, inner) => {
                let variant = hir.datas
                    .get_data(**data)
                    .cons
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| **name == *variant)
                    .unwrap()
                    .0;
                mir::Expr::Variant(variant, self.lower_expr(hir, con, inner))
            },
            hir::Expr::Access(record, field) => {
                let (record_ty, _, indirections) = con.follow_field_access(hir, *record.meta(), **field).unwrap();
                let field_idx = if let ConTy::Record(fields) = con.get_ty(record_ty) {
                    let mut fields = fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();
                    fields.sort_by_key(|name| name.as_ref());
                    fields.iter().enumerate().find(|(_, name)| **name == **field).unwrap().0
                } else {
                    unreachable!();
                };
                let mut record = self.lower_expr(hir, con, record);
                // Perform indirections for field accesses
                for _ in 0..indirections {
                    let variant_repr = if let Repr::Data(data) = record.meta() {
                        if let Repr::Sum(variants) = self.reprs.get(*data) {
                            variants[0].clone()
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    };
                    record = MirNode::new(mir::Expr::AccessVariant(record, 0), variant_repr);
                }

                mir::Expr::Access(record, field_idx)
            },
            hir::Expr::Record(fields) => {
                let mut fields = fields
                    .iter()
                    .map(|(name, field)| (**name, self.lower_expr(hir, con, field)))
                    .collect::<Vec<_>>();
                fields.sort_by_key(|(name, _)| name.as_ref());
                mir::Expr::Tuple(fields.into_iter().map(|(_, field)| field).collect())
            },
            hir::Expr::ClassAccess(ty, class, field) => panic!("Class access should not still exist during MIR lowering"),
            hir::Expr::Debug(inner) => mir::Expr::Debug(self.lower_expr(hir, con, inner)),
            hir::Expr::Intrinsic(name, args) => {
                match name.inner() {
                    hir::Intrinsic::TypeName => {
                        let name = match con.get_ty(*args.first().expect("type_name intrinsic must have an argument").meta()) {
                            ConTy::List(inner) => con.display(hir, *inner).to_string(),
                            _ => panic!("type_name argument must be list of type"),
                        };
                        mir::Expr::Const(Const::Str(Intern::new(name)))
                    },
                }
            },
        };

        MirNode::new(expr, self.lower_ty(hir, con, *con_expr.meta()))
    }
}
