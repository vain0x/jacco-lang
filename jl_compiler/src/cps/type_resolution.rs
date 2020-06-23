//! 型推論・型検査

use super::*;
use std::mem::{swap, take};

/// Typing context.
struct Tx<'a> {
    // 現在の関数の型環境
    ty_env: KTyEnv,
    // 現在の関数に含まれるローカル変数の情報
    locals: Vec<KLocalData>,
    // 現在の関数に含まれるラベルのシグネチャ情報
    label_sigs: Vec<KLabelSig>,
    // 現在の関数の return ラベルの型
    return_ty_opt: Option<KTy>,
    outlines: &'a KOutlines,
    logger: Logger,
}

impl<'a> Tx<'a> {
    fn new(outlines: &'a KOutlines, logger: Logger) -> Self {
        Self {
            ty_env: KTyEnv::default(),
            locals: Default::default(),
            label_sigs: Default::default(),
            return_ty_opt: None,
            outlines,
            logger,
        }
    }

    fn fresh_meta_ty(&mut self, location: Location) -> KTy {
        let meta_ty = self.ty_env.meta_ty_new(location);
        KTy::Meta(meta_ty)
    }
}

fn do_unify(left: &KTy, right: &KTy, location: &Location, tx: &Tx) {
    match (left, right) {
        (KTy::Never, _) | (_, KTy::Never) => {}

        (KTy::Unresolved, other) | (other, KTy::Unresolved) => {
            unreachable!("don't try to unify unresolved meta tys (other={:?})", other)
        }
        (KTy::Meta(left), KTy::Meta(right)) if left == right => {}
        (KTy::Meta(mut meta), other) | (other, KTy::Meta(mut meta)) => {
            match meta.try_unwrap(&tx.ty_env) {
                Some(ty) => {
                    do_unify(&*ty.borrow(), other, location, tx);
                }
                None => {
                    // FIXME: occurrence check
                    meta.bind(other.clone(), &tx.ty_env);
                }
            }
        }

        (KTy::Unit, KTy::Unit)
        | (KTy::I32, KTy::I32)
        | (KTy::I64, KTy::I64)
        | (KTy::Usize, KTy::Usize)
        | (KTy::F64, KTy::F64)
        | (KTy::C8, KTy::C8)
        | (KTy::Bool, KTy::Bool) => {}

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

        (KTy::Struct(left), KTy::Struct(right)) if left == right => {}

        (KTy::Unit, _)
        | (KTy::I32, _)
        | (KTy::I64, _)
        | (KTy::Usize, _)
        | (KTy::F64, _)
        | (KTy::C8, _)
        | (KTy::Bool, _)
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

fn resolve_symbol_def(symbol: &mut KSymbol, expected_ty_opt: Option<&KTy>, tx: &mut Tx) {
    if symbol.ty(&tx.locals).is_unresolved() {
        let expected_ty = match expected_ty_opt {
            None => tx.fresh_meta_ty(symbol.location()),
            Some(ty) => ty.clone(),
        };

        *symbol.ty_mut(&mut tx.locals) = expected_ty;
        return;
    }

    if let Some(expected_ty) = expected_ty_opt {
        let symbol_ty = symbol.ty(&tx.locals);
        do_unify(&symbol_ty, expected_ty, &symbol.location, tx);
    }
}

fn resolve_symbol_use(symbol: &mut KSymbol, tx: &mut Tx) -> KTy {
    let current_ty = symbol.ty(&tx.locals);
    if current_ty.is_unresolved() {
        error!("def_ty is unresolved. symbol is undefined? {:?}", symbol);
    }

    current_ty
}

fn resolve_term(term: &mut KTerm, tx: &mut Tx) -> KTy {
    match term {
        KTerm::Unit { .. } => KTy::Unit,
        KTerm::Int(token, ty) => {
            if ty.is_unresolved() {
                if token.text().ends_with("i64") {
                    *ty = KTy::I64;
                } else if token.text().ends_with("usize") {
                    *ty = KTy::Usize;
                } else {
                    // FIXME: untyped int
                    *ty = KTy::I32
                }
            }
            assert!(ty.is_primitive());
            ty.clone()
        }
        KTerm::Float(_) => KTy::F64,
        KTerm::Char(_) => KTy::C8,
        KTerm::Str(_) => KTy::C8.into_ptr(),
        KTerm::True(_) | KTerm::False(_) => KTy::Bool,
        KTerm::Const(k_const) => k_const.ty(&tx.outlines.consts).clone(),
        KTerm::Fn(k_fn) => k_fn.ty(&tx.outlines),
        KTerm::Label(label) => tx.label_sigs[label.id()].ty(),
        KTerm::Return(_) => tx.return_ty_opt.clone().unwrap(),
        KTerm::ExternFn(extern_fn) => extern_fn.ty(&tx.outlines),
        KTerm::Name(symbol) => resolve_symbol_use(symbol, tx),
        KTerm::FieldTag(_) => unreachable!(),
    }
}

fn resolve_terms(terms: &mut [KTerm], tx: &mut Tx) -> Vec<KTy> {
    terms
        .iter_mut()
        .map(|term| resolve_term(term, tx))
        .collect()
}

fn resolve_node(node: &mut KNode, tx: &mut Tx) {
    match node.prim {
        KPrim::Stuck => {}
        KPrim::Jump => match node.args.as_mut_slice() {
            [label, args @ ..] => {
                let def_fn_ty = resolve_term(label, tx);
                let arg_tys = resolve_terms(args, tx);
                let use_fn_ty = KTy::Fn {
                    param_tys: arg_tys,
                    result_ty: Box::new(KTy::Never),
                };

                unify(def_fn_ty, use_fn_ty, node.location.clone(), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([callee, args @ ..], [result]) => {
                resolve_symbol_def(result, None, tx);

                let def_fn_ty = resolve_term(callee, tx);
                let arg_tys = resolve_terms(args, tx);
                let use_fn_ty = KTy::Fn {
                    param_tys: arg_tys,
                    result_ty: Box::new(result.ty(&tx.locals).to_owned()),
                };

                unify(def_fn_ty, use_fn_ty, node.location.clone(), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Struct => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let k_struct = ty.clone().as_struct().unwrap();
                let outlines = tx.outlines.clone();

                for (arg, field) in node.args.iter_mut().zip(k_struct.fields(&outlines)) {
                    let arg_ty = resolve_term(arg, tx);
                    unify(
                        arg_ty,
                        field.ty(&tx.outlines).clone(),
                        node.location.clone(),
                        tx,
                    );
                }

                resolve_symbol_def(result, Some(ty), tx);

                if !ty.is_struct() {
                    tx.logger.error(result, "struct type required");
                }
            }
            _ => unimplemented!("{:?}", node),
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
                    let k_struct = left_ty.as_ptr()?.as_struct()?;
                    k_struct
                        .fields(&tx.outlines)
                        .iter()
                        .find(|field| field.name(&tx.outlines) == *field_name)
                        .map(|field| field.ty(&tx.outlines).clone().into_ptr())
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
                unify(cond_ty, KTy::Bool, node.location.clone(), tx);
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
        KPrim::Cast => match (
            node.tys.as_mut_slice(),
            node.args.as_mut_slice(),
            node.results.as_mut_slice(),
        ) {
            ([ty], [arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_symbol_def(result, Some(ty), tx);

                // FIXME: 同じ型へのキャストは警告?
                if let KTy::Unresolved | KTy::Never = arg_ty {
                    // Skip.
                } else if !arg_ty.is_primitive() {
                    tx.logger.error(node, "can't cast from non-primitive type");
                } else if !ty.is_primitive() {
                    tx.logger.error(node, "can't cast to non-primitive type");
                }
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
        KPrim::Add | KPrim::Sub => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, right], [result]) => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);

                if tx.ty_env.is_ptr(&left_ty) {
                    unify(right_ty.clone(), KTy::Usize, node.location.clone(), tx);
                } else {
                    // FIXME: iNN or uNN
                    unify(left_ty.clone(), right_ty, node.location.clone(), tx);
                }

                resolve_symbol_def(result, Some(&left_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Mul
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
                unify(left_ty.clone(), right_ty, node.location.clone(), tx);

                resolve_symbol_def(result, Some(&left_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Eq | KPrim::Ne | KPrim::Lt | KPrim::Le | KPrim::Gt | KPrim::Ge => {
            match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
                ([left, right], [result]) => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);
                    // FIXME: Eq/Ord only
                    unify(left_ty, right_ty, node.location.clone(), tx);

                    resolve_symbol_def(result, Some(&KTy::Bool), tx);
                }
                _ => unimplemented!(),
            }
        }
        KPrim::Assign => match node.args.as_mut_slice() {
            [left, right] => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);
                unify(left_ty, right_ty.into_ptr(), node.location.clone(), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::AddAssign | KPrim::SubAssign => {
            match node.args.as_mut_slice() {
                [left, right] => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);

                    // 左辺は lval なのでポインタ型のはず
                    let left_ty = tx.ty_env.as_ptr(&left_ty).unwrap();

                    // FIXME: add/sub と同じ
                    if tx.ty_env.is_ptr(&left_ty) {
                        unify(right_ty.clone(), KTy::Usize, node.location.clone(), tx);
                    } else {
                        // FIXME: iNN or uNN
                        unify(left_ty.clone(), right_ty, node.location.clone(), tx);
                    }
                }
                _ => unimplemented!(),
            }
        }
        KPrim::MulAssign
        | KPrim::DivAssign
        | KPrim::ModuloAssign
        | KPrim::BitAndAssign
        | KPrim::BitOrAssign
        | KPrim::BitXorAssign
        | KPrim::LeftShiftAssign
        | KPrim::RightShiftAssign => {
            match node.args.as_mut_slice() {
                [left, right] => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);

                    // 左辺は lval なのでポインタ型のはず
                    let left_ty = tx.ty_env.as_ptr(&left_ty).unwrap();

                    // FIXME: mul/div/etc. と同じ
                    // FIXME: iNN or uNN
                    unify(left_ty.clone(), right_ty, node.location.clone(), tx);
                }
                _ => unimplemented!(),
            }
        }
    }

    for cont in &mut node.conts {
        resolve_node(cont, tx);
    }
}

fn prepare_fn(k_fn: KFn, fn_data: &mut KFnData, tx: &mut Tx) {
    let outlines = tx.outlines.clone();

    assert!(tx.ty_env.is_empty());
    assert!(fn_data.ty_env.is_empty());

    for i in 0..fn_data.params.len() {
        let param = &mut fn_data.params[i];
        let param_ty = &k_fn.param_tys(&outlines)[i];
        resolve_symbol_def(param, Some(param_ty), tx);
    }

    // いまから生成するところなので空のはず。
    assert!(fn_data.label_sigs.is_empty());

    for label_data in &mut fn_data.labels {
        for param in &mut label_data.params {
            resolve_symbol_def(param, None, tx);
        }

        let label_sig = {
            let name = label_data.name.to_string();
            let param_tys = label_data
                .params
                .iter()
                .map(|param| param.ty(&tx.locals))
                .collect();
            KLabelSig::new(name, param_tys)
        };
        fn_data.label_sigs.push(label_sig);
    }

    assert_eq!(fn_data.label_sigs.len(), fn_data.labels.len());

    fn_data.ty_env = take(&mut tx.ty_env);
}

fn prepare_extern_fn(extern_fn: KExternFn, data: &mut KExternFnData, tx: &mut Tx) {
    let outlines = tx.outlines.clone();

    for i in 0..data.params.len() {
        let param = &mut data.params[i];
        let param_ty = &extern_fn.param_tys(&outlines)[i];
        resolve_symbol_def(param, Some(param_ty), tx);
    }
}

fn prepare_struct(k_struct: KStruct, tx: &mut Tx) {
    for field in k_struct.fields(&tx.outlines) {
        if field.ty(&tx.outlines).is_unresolved() {
            // FIXME: handle correctly. unresolved type crashes on unification for now
            tx.logger
                .error(&field.location(&tx.outlines), "unresolved field type");
        }
    }
}

fn resolve_root(root: &mut KRoot, tx: &mut Tx) {
    let outlines = tx.outlines.clone();

    for k_struct in outlines.structs_iter() {
        prepare_struct(k_struct, tx);
    }

    for extern_fn in outlines.extern_fns_iter() {
        let extern_fn_data = &mut root.extern_fns[extern_fn.id()];
        swap(&mut tx.locals, &mut extern_fn_data.locals);

        prepare_extern_fn(extern_fn, extern_fn_data, tx);

        swap(&mut tx.locals, &mut extern_fn_data.locals);
    }

    for k_fn in outlines.fns_iter() {
        let fn_data = &mut root.fns[k_fn.id()];
        swap(&mut tx.locals, &mut fn_data.locals);

        prepare_fn(k_fn, fn_data, tx);

        swap(&mut tx.locals, &mut fn_data.locals);
    }

    // 項の型を解決する。
    for k_fn in outlines.fns_iter() {
        let fn_data = &mut root.fns[k_fn.id()];

        tx.return_ty_opt = Some(k_fn.return_ty(&outlines));
        swap(&mut tx.locals, &mut fn_data.locals);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);

        resolve_node(&mut fn_data.body, tx);

        for label in &mut fn_data.labels {
            resolve_node(&mut label.body, tx);
        }

        for local_data in &mut tx.locals {
            if tx.ty_env.is_unbound(&local_data.ty) {
                local_data.ty = KTy::Never;
            }
        }

        swap(&mut tx.locals, &mut fn_data.locals);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);
        tx.return_ty_opt.take();
    }
}

pub(crate) fn resolve_types(k_root: &mut KRoot, logger: Logger) {
    let outlines = take(&mut k_root.outlines);

    let mut tx = Tx::new(&outlines, logger);
    resolve_root(k_root, &mut tx);

    k_root.outlines = outlines;
}
