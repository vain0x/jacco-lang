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

fn constraint_ty(symbol: &mut KSymbol, ty: KTy, tx: &mut Tx) -> KTy {
    let symbol_ref = symbol.as_symbol_ref();
    match tx.symbol_tys.get(&symbol_ref) {
        Some(ty) if *ty != KTy::Unresolved => {
            // FIXME: 型検査
            symbol.ty = ty.clone();
        }
        _ if ty != KTy::Unresolved => {
            tx.symbol_tys.insert(symbol_ref, ty.clone());
            symbol.ty = ty;
        }
        _ => {}
    }
    symbol.ty.clone()
}

fn resolve_term(term: &mut KTerm, expected_ty: KTy, tx: &mut Tx) -> KTy {
    match term {
        KTerm::Unit { .. } => KTy::Unit,
        KTerm::Int(_) => KTy::I32,
        KTerm::Name(symbol) => constraint_ty(symbol, expected_ty, tx),
    }
}

fn resolve_node(node: &mut KNode, tx: &mut Tx) {
    for arg in &mut node.args {
        resolve_term(arg, KTy::Unresolved, tx);
    }

    match node.prim {
        KPrim::Stuck => {}
        KPrim::Jump => match node.args.as_mut_slice() {
            [KTerm::Name(label), args @ ..] => {
                if let Some(KTy::Fn { param_tys, .. }) = tx.symbol_tys.get(&label.as_symbol_ref()) {
                    let param_tys = param_tys.to_owned();
                    for (param_ty, arg) in param_tys.into_iter().zip(args.iter_mut()) {
                        resolve_term(arg, param_ty, tx);
                    }
                }
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect | KPrim::If => {}
        KPrim::Let => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([init], [result]) => {
                let init_ty = resolve_term(init, KTy::Unresolved, tx);
                result.ty = init_ty;
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
        | KPrim::RightShift => match node.results.as_mut_slice() {
            [result] => {
                constraint_ty(result, KTy::I32, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Eq | KPrim::Ne | KPrim::Lt | KPrim::Le | KPrim::Gt | KPrim::Ge => {
            match node.results.as_mut_slice() {
                [result] => {
                    constraint_ty(result, KTy::I32, tx); // FIXME: bool
                }
                _ => unimplemented!(),
            }
        }
    }

    for cont in &mut node.conts {
        resolve_node(cont, tx);
    }
}

fn resolve_fn_sig(fn_symbol: &KSymbol, params: &[KSymbol], tx: &mut Tx) {
    let mut param_tys = vec![];

    for param in params {
        tx.symbol_tys
            .insert(param.as_symbol_ref(), param.ty.clone());
        param_tys.push(param.ty.clone());
    }

    tx.symbol_tys.insert(
        fn_symbol.as_symbol_ref(),
        KTy::Fn {
            param_tys,
            result_ty: Box::new(KTy::Never),
        },
    );
}

fn resolve_root(root: &mut KRoot, tx: &mut Tx) {
    // 関数とパラメータの型情報をシンボルテーブルに登録する。
    for k_fn in &root.fns {
        resolve_fn_sig(&k_fn.name, &k_fn.params, tx);

        for label in &k_fn.labels {
            resolve_fn_sig(&label.name, &label.params, tx);
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
