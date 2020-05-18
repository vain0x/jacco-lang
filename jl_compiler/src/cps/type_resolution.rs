//! 型推論・型検査

use super::*;
use std::rc::Rc;

#[derive(Default)]
struct InitMetaTys;

impl InitMetaTys {
    fn on_symbol_def(&mut self, symbol: &mut KSymbol) {
        if let KTy::Unresolved(None) = symbol.ty {
            let meta = KMetaTy::new(symbol.location.clone());
            symbol.ty = KTy::Unresolved(Some(meta));
        }

        let mut def_ty_slot = symbol.def_ty_slot().borrow_mut();
        *def_ty_slot = symbol.ty.clone();
    }

    fn on_node(&mut self, node: &mut KNode) {
        for result in &mut node.results {
            self.on_symbol_def(result);
        }

        for cont in &mut node.conts {
            self.on_node(cont);
        }
    }

    fn on_fn(&mut self, k_fn: &mut KFn) {
        self.on_symbol_def(&mut k_fn.name);

        for param in &mut k_fn.params {
            self.on_symbol_def(param);
        }

        self.on_node(&mut k_fn.body);

        for label in &mut k_fn.labels {
            self.on_fn(label);
        }
    }

    fn execute(mut self, root: &mut KRoot, _tx: &mut Tx) {
        for k_fn in &mut root.fns {
            self.on_fn(k_fn);
        }

        for extern_fn in &mut root.extern_fns {
            self.on_symbol_def(&mut extern_fn.name);

            for param in &mut extern_fn.params {
                self.on_symbol_def(param);
            }
        }

        for k_struct in &mut root.structs {
            let mut def = k_struct.def.borrow_mut();
            self.on_symbol_def(&mut def.name);

            for field in &mut def.fields {
                self.on_symbol_def(&mut field.name);
            }
        }
    }
}

/// Typing context.
struct Tx {
    logger: Logger,
}

impl Tx {
    fn new(logger: Logger) -> Self {
        Tx { logger }
    }
}

fn unify(left: KTy, right: KTy, location: Location, tx: &mut Tx) {
    match (left, right) {
        (KTy::Never, _) | (_, KTy::Never) => {}

        (KTy::Unresolved(None), other) | (other, KTy::Unresolved(None)) => {
            unreachable!("don't try to unify unresolved meta tys (other={:?})", other)
        }
        (KTy::Unresolved(Some(left)), KTy::Unresolved(Some(right))) if left.ptr_eq(&right) => {}
        (KTy::Unresolved(Some(mut meta)), other) | (other, KTy::Unresolved(Some(mut meta))) => {
            if meta.is_bound() {
                unify(meta.content_ty().unwrap(), other, location, tx);
            } else {
                // FIXME: occurrence check
                meta.bind(other);
            }
        }

        (KTy::Unit, KTy::Unit) | (KTy::I32, KTy::I32) => {}

        (KTy::Ptr { ty: left }, KTy::Ptr { ty: right }) => {
            unify(*left, *right, location, tx);
        }

        (
            KTy::Fn {
                param_tys: left_param_tys,
                result_ty: left_result_ty,
            },
            KTy::Fn {
                param_tys: right_param_tys,
                result_ty: right_result_ty,
            },
        ) => {
            unify(*left_result_ty, *right_result_ty, location.clone(), tx);

            let left_param_count = left_param_tys.len();
            let right_param_count = right_param_tys.len();

            for (left, right) in left_param_tys.into_iter().zip(right_param_tys) {
                unify(left, right, location.clone(), tx);
            }

            if left_param_count != right_param_count {
                // NOTE: unify types as possible even if param counts don't match.
                tx.logger.error(location, "arity mismatch");
            }
        }

        (KTy::Symbol { def: left_def }, KTy::Symbol { def: right_def })
            if Rc::ptr_eq(&left_def, &right_def) => {}

        (KTy::Unit, _)
        | (KTy::I32, _)
        | (KTy::Ptr { .. }, _)
        | (KTy::Fn { .. }, _)
        | (KTy::Symbol { .. }, _) => {
            tx.logger.error(location, "type mismatch");
        }
    }
}

fn resolve_symbol_use(symbol: &mut KSymbol, _tx: &mut Tx) -> KTy {
    let current_ty = symbol.def_ty_slot().borrow().clone();
    symbol.ty = current_ty.clone();
    current_ty
}

fn resolve_term(term: &mut KTerm, tx: &mut Tx) -> KTy {
    match term {
        KTerm::Unit { .. } => KTy::Unit,
        KTerm::Int(_) => KTy::I32,
        KTerm::Name(symbol) => resolve_symbol_use(symbol, tx),
        KTerm::Field { .. } => KTy::new_unresolved(),
    }
}

fn resolve_node(node: &mut KNode, tx: &mut Tx) {
    let location = Location::default(); // FIXME: node needs location?

    match node.prim {
        KPrim::Stuck => {}
        KPrim::Jump => match node.args.as_mut_slice() {
            [KTerm::Name(label), args @ ..] => {
                let def_fn_ty = resolve_symbol_use(label, tx);

                let arg_tys = args
                    .iter_mut()
                    .map(|arg| resolve_term(arg, tx))
                    .collect::<Vec<_>>();
                let use_fn_ty = KTy::Fn {
                    param_tys: arg_tys,
                    result_ty: Box::new(KTy::Never),
                };

                unify(def_fn_ty, use_fn_ty, location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect => {
            for arg in &mut node.args {
                resolve_term(arg, tx);
            }
        }
        KPrim::Struct => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let struct_def = match ty {
                    KTy::Symbol { def } => def.clone(),
                    _ => unimplemented!(),
                };

                for (arg, field_def) in node.args.iter_mut().zip(&struct_def.borrow().fields) {
                    let arg_ty = resolve_term(arg, tx);
                    unify(arg_ty, field_def.name.ty.clone(), location.clone(), tx);
                }

                unify(ty.clone(), result.ty.clone(), location, tx);
                if !ty.is_symbol() {
                    tx.logger
                        .error(result.location.clone(), "symbol type required");
                }
            }
            _ => unimplemented!(),
        },
        KPrim::GetField => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            (
                [left, KTerm::Field {
                    text: field_name,
                    location,
                }],
                [result],
            ) => {
                let left_ty = resolve_term(left, tx);

                if let Some(def) = match left_ty.resolve() {
                    KTy::Ptr { ty } => match ty.resolve() {
                        KTy::Symbol { def } => Some(def),
                        _ => None,
                    },
                    _ => None,
                } {
                    if let Some(field) = def
                        .borrow()
                        .fields
                        .iter()
                        .find(|field| field.name.text == *field_name)
                    {
                        unify(
                            field.name.ty.clone().into_ptr(),
                            result.ty.clone(),
                            location.clone(),
                            tx,
                        );
                    } else {
                        tx.logger.error(location.clone(), "unknown field");
                    }
                }
            }
            _ => unimplemented!(),
        },
        KPrim::If => match node.args.as_mut_slice() {
            [cond] => {
                let cond_ty = resolve_term(cond, tx);
                unify(cond_ty, KTy::I32, location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Let => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([init], [result]) => {
                let init_ty = resolve_term(init, tx);
                unify(init_ty, result.ty.clone(), location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                unify(arg_ty, result.ty.clone().into_ptr(), location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Ref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                unify(arg_ty.into_ptr(), result.ty.clone(), location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Minus => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                unify(arg_ty, result.ty.clone(), location, tx);
                // FIXME: bool or iNN
            }
            _ => unimplemented!(),
        },
        KPrim::Negate => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                unify(arg_ty, result.ty.clone(), location, tx);
                // FIXME: bool or iNN or uNN
            }
            _ => unimplemented!(),
        },
        KPrim::Add
        | KPrim::Sub
        | KPrim::Mul
        | KPrim::Div
        | KPrim::Mod
        | KPrim::BitAnd
        | KPrim::BitOr
        | KPrim::BitXor
        | KPrim::LeftShift
        | KPrim::RightShift => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, right], [result]) => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);
                unify(left_ty, right_ty, location.clone(), tx);
                unify(result.ty.clone(), KTy::I32, location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Eq | KPrim::Ne | KPrim::Lt | KPrim::Le | KPrim::Gt | KPrim::Ge => {
            match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
                ([left, right], [result]) => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);
                    unify(left_ty, right_ty, location.clone(), tx);
                    unify(result.ty.clone(), KTy::I32, location, tx); // FIXME: bool
                }
                _ => unimplemented!(),
            }
        }
        KPrim::Assign => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, right], [result]) => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);
                unify(left_ty, right_ty.into_ptr(), location.clone(), tx);
                unify(result.ty.clone(), KTy::Unit, location, tx);
            }
            _ => unimplemented!(),
        },
    }

    for cont in &mut node.conts {
        resolve_node(cont, tx);
    }
}

fn resolve_fn_sig(fn_symbol: &mut KSymbol, params: &[KSymbol], tx: &mut Tx) {
    let fn_ty = KTy::Fn {
        param_tys: params.iter().map(|param| param.ty.clone()).collect(),
        result_ty: Box::new(KTy::Never),
    };

    let location = fn_symbol.location.clone();
    unify(fn_symbol.ty.clone(), fn_ty, location, tx)
}

fn resolve_root(root: &mut KRoot, tx: &mut Tx) {
    InitMetaTys::default().execute(root, tx);

    for k_fn in &mut root.fns {
        resolve_fn_sig(&mut k_fn.name, &k_fn.params, tx);

        for label in &mut k_fn.labels {
            resolve_fn_sig(&mut label.name, &label.params, tx);
        }
    }

    for extern_fn in &mut root.extern_fns {
        resolve_fn_sig(&mut extern_fn.name, &extern_fn.params, tx);
    }

    for k_struct in &mut root.structs {
        let def = &k_struct.def;
        let ty = KTy::Symbol { def: def.clone() };
        *def.borrow_mut().name.def_ty_slot().borrow_mut() = ty.clone();
        def.borrow_mut().name.ty = ty;
    }

    // 項の型を解決する。
    for k_fn in &mut root.fns {
        resolve_node(&mut k_fn.body, tx);

        for label in &mut k_fn.labels {
            resolve_node(&mut label.body, tx);
        }
    }
}

pub(crate) fn resolve_types(k_root: &mut KRoot, logger: Logger) {
    let mut tx = Tx::new(logger);
    resolve_root(k_root, &mut tx);
}
