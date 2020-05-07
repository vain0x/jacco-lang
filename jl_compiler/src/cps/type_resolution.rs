//! 型推論・型検査

use super::*;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct KSymbolRef {
    id: usize,
}

impl KSymbol {
    fn as_symbol_ref(&self) -> KSymbolRef {
        KSymbolRef { id: self.id }
    }
}

#[derive(Default)]
struct InitMetaTys {
    env: HashMap<KSymbolRef, KTy>,
}

impl InitMetaTys {
    fn on_symbol_def(&mut self, symbol: &mut KSymbol) {
        if let KTy::Unresolved(None) = symbol.ty {
            let meta = KMetaTy::new(symbol.location.clone());
            symbol.ty = KTy::Unresolved(Some(meta));
        }

        self.env.insert(symbol.as_symbol_ref(), symbol.ty.clone());
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
        for param in &mut k_fn.params {
            self.on_symbol_def(param);
        }

        self.on_node(&mut k_fn.body);

        for label in &mut k_fn.labels {
            self.on_fn(label);
        }
    }

    fn execute(mut self, root: &mut KRoot, tx: &mut Tx) {
        for k_fn in &mut root.fns {
            self.on_fn(k_fn);
        }

        tx.symbol_tys = self.env;
    }
}

/// Typing context.
struct Tx {
    symbol_tys: HashMap<KSymbolRef, KTy>,
    #[allow(dead_code)]
    logger: Logger,
}

impl Tx {
    fn new(logger: Logger) -> Self {
        Tx {
            symbol_tys: HashMap::default(),
            logger,
        }
    }
}

fn unify(left: KTy, right: KTy, location: Location, tx: &mut Tx) {
    match (left, right) {
        (KTy::Never, _) | (_, KTy::Never) => {}

        (KTy::Unresolved(None), _) | (_, KTy::Unresolved(None)) => {
            unreachable!("don't try to unify unresolved meta tys")
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

        (KTy::Unit, _) | (KTy::I32, _) | (KTy::Fn { .. }, _) => {
            tx.logger.error(location, "type mismatch");
        }
    }
}

fn constrain_symbol_ty(symbol: &mut KSymbol, required_ty: KTy, tx: &mut Tx) {
    let symbol_ref = symbol.as_symbol_ref();

    match tx.symbol_tys.get(&symbol_ref) {
        Some(current_ty) => {
            symbol.ty = current_ty.clone();
            unify(current_ty.clone(), required_ty, symbol.location.clone(), tx);
        }
        None => {
            symbol.ty = required_ty.clone();
            tx.symbol_tys.insert(symbol_ref, required_ty);
        }
    }
}

fn resolve_symbol_use(symbol: &mut KSymbol, tx: &mut Tx) -> KTy {
    if let Some(ty) = tx.symbol_tys.get(&symbol.as_symbol_ref()) {
        constrain_symbol_ty(symbol, ty.clone(), tx);
    }

    symbol.ty.clone()
}

fn resolve_term(term: &mut KTerm, tx: &mut Tx) -> KTy {
    match term {
        KTerm::Unit { .. } => KTy::Unit,
        KTerm::Int(_) => KTy::I32,
        KTerm::Name(symbol) => resolve_symbol_use(symbol, tx),
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
            _ => {}
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
    }

    for cont in &mut node.conts {
        resolve_node(cont, tx);
    }
}

fn resolve_fn_sig(fn_symbol: &mut KSymbol, params: &mut [KSymbol], tx: &mut Tx) {
    for param in params.iter_mut() {
        tx.symbol_tys
            .insert(param.as_symbol_ref(), param.ty.clone());
    }

    let fn_ty = KTy::Fn {
        param_tys: params.iter().map(|param| param.ty.clone()).collect(),
        result_ty: Box::new(KTy::Never),
    };

    fn_symbol.ty = fn_ty.clone();
    tx.symbol_tys.insert(fn_symbol.as_symbol_ref(), fn_ty);
}

fn resolve_root(root: &mut KRoot, tx: &mut Tx) {
    InitMetaTys::default().execute(root, tx);

    // 関数とパラメータの型情報をシンボルテーブルに登録する。
    for k_fn in &mut root.fns {
        resolve_fn_sig(&mut k_fn.name, &mut k_fn.params, tx);

        for label in &mut k_fn.labels {
            resolve_fn_sig(&mut label.name, &mut label.params, tx);
        }
    }

    // for extern_fn in &mut root.extern_fns {
    //     resolve_fn_params(&mut extern_fn.params, tx);
    // }

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
