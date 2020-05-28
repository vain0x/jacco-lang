//! 型推論・型検査

use super::*;
use std::rc::Rc;

/// Typing context.
struct Tx {
    logger: Logger,
}

impl Tx {
    fn new(logger: Logger) -> Self {
        Tx { logger }
    }
}

fn do_unify(left: &KTy, right: &KTy, location: &Location, tx: &mut Tx) {
    match (left, right) {
        (KTy::Never, _) | (_, KTy::Never) => {}

        (KTy::Unresolved, other) | (other, KTy::Unresolved) => {
            unreachable!("don't try to unify unresolved meta tys (other={:?})", other)
        }
        (KTy::Meta(left), KTy::Meta(right)) if Rc::ptr_eq(left, right) => {}
        (KTy::Meta(meta), other) | (other, KTy::Meta(meta)) => {
            match meta.try_resolve() {
                Some(ty) => {
                    do_unify(&ty, other, location, tx);
                }
                None => {
                    // FIXME: occurrence check
                    meta.bind(other.clone());
                }
            }
        }

        (KTy::Unit, KTy::Unit) | (KTy::I32, KTy::I32) => {}

        (KTy::Ptr { ty: left }, KTy::Ptr { ty: right }) => {
            do_unify(left, right, location, tx);
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
            do_unify(left_result_ty, right_result_ty, location, tx);

            let left_param_count = left_param_tys.len();
            let right_param_count = right_param_tys.len();

            for (left, right) in left_param_tys.into_iter().zip(right_param_tys) {
                do_unify(left, right, location, tx);
            }

            if left_param_count != right_param_count {
                // NOTE: unify types as possible even if param counts don't match.
                tx.logger.error(location, "arity mismatch");
            }
        }

        (KTy::Struct { struct_ref: left }, KTy::Struct { struct_ref: right })
            if left.is_same(right) => {}

        (KTy::Unit, _)
        | (KTy::I32, _)
        | (KTy::Ptr { .. }, _)
        | (KTy::Fn { .. }, _)
        | (KTy::Struct { .. }, _) => {
            tx.logger.error(location, "type mismatch");
        }
    }
}

fn unify(left: KTy, right: KTy, location: Location, tx: &mut Tx) {
    do_unify(&left, &right, &location, tx);
}

fn resolve_symbol_def(symbol: &KSymbol, expected_ty_opt: Option<&KTy>, tx: &mut Tx) {
    if symbol.def.ty.borrow().is_unresolved() {
        let expected_ty = match expected_ty_opt {
            None => {
                let meta = KMetaTy::new(KMetaTyData::new(symbol.location.clone()));
                KTy::Meta(meta)
            }
            Some(ty) => ty.clone(),
        };

        *symbol.def.ty.borrow_mut() = expected_ty;
        return;
    }

    if let Some(expected_ty) = expected_ty_opt {
        let symbol_ty = symbol.def.ty.borrow();
        do_unify(&symbol_ty, expected_ty, &symbol.location, tx);
    }
}

fn resolve_symbol_use(symbol: &mut KSymbol, _tx: &mut Tx) -> KTy {
    let current_ty = symbol.def_ty_slot().borrow().clone();
    if current_ty.is_unresolved() {
        error!(
            "def_ty is unresolved. symbol is undefined? {:?}",
            symbol.location
        );
    }

    current_ty
}

fn resolve_term(term: &mut KTerm, tx: &mut Tx) -> KTy {
    match term {
        KTerm::Unit { .. } => KTy::Unit,
        KTerm::Int(_) => KTy::I32,
        KTerm::Name(symbol) => resolve_symbol_use(symbol, tx),
        KTerm::FieldTag(_) => unreachable!(),
    }
}

fn resolve_node(node: &mut KNode, tx: &mut Tx) {
    let location = Location::default(); // FIXME: node needs location?

    match node.prim {
        KPrim::Stuck => {}
        KPrim::Jump => match node.args.as_mut_slice() {
            [label, args @ ..] => {
                let def_fn_ty = resolve_term(label, tx);

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
        KPrim::CallDirect => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([callee, args @ ..], [result]) => {
                let def_fn_ty = resolve_term(callee, tx);
                resolve_symbol_def(result, None, tx);

                let arg_tys = args
                    .iter_mut()
                    .map(|arg| resolve_term(arg, tx))
                    .collect::<Vec<_>>();
                let use_fn_ty = KTy::Fn {
                    param_tys: arg_tys,
                    result_ty: Box::new(result.ty()),
                };

                unify(def_fn_ty, use_fn_ty, location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Struct => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let struct_def = match ty {
                    KTy::Struct { struct_ref } => struct_ref.def.clone(),
                    _ => unimplemented!(),
                };

                for (arg, field_def) in node.args.iter_mut().zip(&struct_def.fields) {
                    let arg_ty = resolve_term(arg, tx);
                    unify(arg_ty, field_def.name.ty(), location.clone(), tx);
                }

                resolve_symbol_def(result, Some(ty), tx);

                if !ty.is_struct() {
                    tx.logger.error(result, "struct type required");
                }
            }
            _ => unimplemented!(),
        },
        KPrim::GetField => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            (
                [left, KTerm::FieldTag(KFieldTag {
                    name: field_name,
                    location,
                })],
                [result],
            ) => {
                let left_ty = resolve_term(left, tx);

                let result_ty = (|| {
                    let k_struct = left_ty.resolve().as_ptr()?.as_struct()?;
                    let field = k_struct
                        .def
                        .fields
                        .iter()
                        .find(|field| field.name.raw_name() == *field_name)?;
                    Some(field.name.ty().into_ptr())
                })()
                .unwrap_or_else(|| {
                    tx.logger.error(location, "bad type");
                    KTy::Never
                });

                resolve_symbol_def(result, Some(&result_ty), tx);
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
                resolve_symbol_def(result, Some(&init_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);

                let result_ty_opt = arg_ty.as_ptr();
                if result_ty_opt.is_none() {
                    tx.logger.error(&result.location, "expected a reference");
                }
                resolve_symbol_def(result, result_ty_opt.as_ref(), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Ref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_symbol_def(result, Some(&arg_ty.into_ptr()), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Minus => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                // FIXME: bool or iNN
                resolve_symbol_def(result, Some(&arg_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Not => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                // FIXME: bool or iNN or uNN
                resolve_symbol_def(result, Some(&arg_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Add
        | KPrim::Sub
        | KPrim::Mul
        | KPrim::Div
        | KPrim::Modulo
        | KPrim::BitAnd
        | KPrim::BitOr
        | KPrim::BitXor
        | KPrim::LeftShift
        | KPrim::RightShift => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, right], [result]) => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);
                // FIXME: iNN or uNN
                unify(left_ty, right_ty, location.clone(), tx);

                // FIXME: left_ty?
                resolve_symbol_def(result, Some(&KTy::I32), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Eq | KPrim::Ne | KPrim::Lt | KPrim::Le | KPrim::Gt | KPrim::Ge => {
            match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
                ([left, right], [result]) => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);
                    // FIXME: Eq/Ord only
                    unify(left_ty, right_ty, location.clone(), tx);

                    // FIXME: bool
                    resolve_symbol_def(result, Some(&KTy::I32), tx);
                }
                _ => unimplemented!(),
            }
        }
        KPrim::Assign => match node.args.as_mut_slice() {
            [left, right] => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);
                unify(left_ty, right_ty.into_ptr(), location.clone(), tx);
            }
            _ => unimplemented!(),
        },
    }

    for cont in &mut node.conts {
        resolve_node(cont, tx);
    }
}

fn prepare_fn_sig(fn_symbol: &mut KSymbol, params: &[KSymbol], result_ty: KTy) {
    let fn_ty = KTy::Fn {
        param_tys: params.iter().map(|param| param.ty()).collect(),
        result_ty: Box::new(result_ty),
    };

    *fn_symbol.def.ty.borrow_mut() = fn_ty;
}

fn prepare_fn(k_fn: &mut KFnData, tx: &mut Tx) {
    for param in &mut k_fn.params {
        resolve_symbol_def(param, None, tx);
    }

    prepare_fn_sig(&mut k_fn.name, &k_fn.params, KTy::Never);

    for label in &mut k_fn.labels {
        prepare_fn(label, tx);
    }
}

fn prepare_extern_fn(extern_fn: &mut KExternFnData, tx: &mut Tx) {
    for param in &mut extern_fn.params {
        resolve_symbol_def(param, None, tx);
    }

    prepare_fn_sig(
        &mut extern_fn.name,
        &extern_fn.params,
        extern_fn.result_ty.clone(),
    );
}

fn prepare_struct(k_struct: &KStruct, tx: &mut Tx) {
    let struct_ty = KTy::Struct {
        struct_ref: k_struct.clone(),
    };

    let struct_data = &k_struct.def;

    *struct_data.symbol.def.ty.borrow_mut() = struct_ty.clone();
    *struct_data.def_site_ty.borrow_mut() = struct_ty;

    for field in &struct_data.fields {
        resolve_symbol_def(&field.name, None, tx);
    }
}

fn resolve_root(root: &mut KRoot, tx: &mut Tx) {
    for k_struct in &root.structs {
        prepare_struct(&k_struct, tx);
    }

    for extern_fn in &mut root.extern_fns {
        prepare_extern_fn(extern_fn, tx);
    }

    for k_fn in &mut root.fns {
        prepare_fn(k_fn, tx);
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
