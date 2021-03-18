//! 型推論・型検査

use super::*;
use crate::{
    cps::{
        k_meta_ty::KMetaTyData,
        k_ty::{KEnumOrStruct, KTy2, KTyCause},
        ty_unification::UnificationContext,
    },
    source::Loc,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    mem::{replace, swap, take},
};

/// Typing context. 型検査の状態
struct Tx<'a> {
    /// 現在の関数の型環境
    ty_env: KTyEnv,

    /// 現在の関数に含まれるローカル変数の情報
    local_vars: KLocalVarArena,

    /// 現在の関数に含まれるラベルのシグネチャ情報
    label_sigs: KLabelSigArena,

    /// ラベルのパラメータにかかる制約 (下界)。
    ///
    /// ラベルにジャンプする際に制約が増える。
    /// ラベルの型検査を始める際に決定する。
    param_bounds: HashMap<KLabel, Vec<KTy2>>,

    /// 現在の関数の return ラベルの型
    return_ty_opt: Option<KTy>,

    /// 検査対象のモジュールのアウトライン
    mod_outline: &'a KModOutline,

    logger: Logger,
}

impl<'a> Tx<'a> {
    fn new(mod_outline: &'a KModOutline, logger: Logger) -> Self {
        Self {
            ty_env: KTyEnv::default(),
            local_vars: Default::default(),
            label_sigs: Default::default(),
            param_bounds: Default::default(),
            return_ty_opt: None,
            mod_outline,
            logger,
        }
    }

    fn take_label_bound(&mut self, label: KLabel, len: usize) -> Vec<KTy2> {
        replace(
            self.param_bounds
                .entry(label)
                .or_insert_with(|| vec![KTy2::Never; len]),
            vec![],
        )
    }
}

/// left 型の変数に right 型の値を代入できるか判定する。
/// 必要に応じて型変数を束縛する。
fn unify2(left: &KTy2, right: &KTy2, loc: Loc, tx: &mut Tx) {
    #[cfg(skip)]
    log::trace!(
        "unify: {} <- {} @{:?}",
        left.display(&tx.ty_env, tx.mod_outline),
        right.display(&tx.ty_env, tx.mod_outline),
        loc,
    );

    UnificationContext::new(loc, &tx.ty_env, tx.mod_outline, &tx.logger).unify(&left, &right);
}

fn fresh_meta_ty(tx: &mut Tx) -> KTy2 {
    let meta_ty = tx.ty_env.alloc(KMetaTyData::new(RefCell::default()));
    KTy2::Meta(meta_ty)
}

fn error_invalid_size_of(loc: Loc, logger: &Logger) {
    logger.error(loc, "この型のサイズは取得できません。".into());
}

fn get_field_ty(struct_ty: &KTy2, field: KField, loc: Loc, tx: &mut Tx) -> KTy2 {
    match struct_ty {
        KTy2::Struct(_) => field.ty(&tx.mod_outline.fields).to_ty2_poly(tx.mod_outline),
        KTy2::App { ty_args, .. } => field
            .ty(&tx.mod_outline.fields)
            .substitute(tx.mod_outline, ty_args),
        _ => {
            tx.logger.error(loc, "レコード型が必要です".into());
            KTy2::Unresolved {
                cause: KTyCause::Loc(loc),
            }
        }
    }
}

fn get_field_or_variant(
    hint: &str,
    field_name: &str,
    left_ty: &KTy2,
    k_mut: KMut,
    loc: Loc,
    tx: &mut Tx,
) -> (Option<KField>, KTy2) {
    let mut trial = || -> Option<(Option<KField>, KTy2)> {
        let (_, ty) = left_ty.as_ptr(&tx.ty_env)?;

        // ジェネリック構造体のケース
        if let KTy2::App { k_struct, .. } = ty {
            let field = *k_struct
                .of(&tx.mod_outline.structs)
                .fields
                .iter()
                .find(|field| field.name(&tx.mod_outline.fields) == field_name)?;

            let field_ty = get_field_ty(&ty, field, loc, tx).into_ptr(k_mut);
            return Some((Some(field), field_ty));
        }

        match ty.as_struct_or_enum(&tx.ty_env)? {
            KEnumOrStruct::Enum(struct_enum) => {
                let k_struct = *struct_enum
                    .variants(&tx.mod_outline.struct_enums)
                    .iter()
                    .find(|&k_struct| k_struct.name(&tx.mod_outline.structs) == field_name)?;

                let ty = KTy2::Struct(k_struct).into_ptr(k_mut);
                Some((None, ty))
            }
            KEnumOrStruct::Struct(k_struct) => {
                let field = *k_struct
                    .fields(&tx.mod_outline.structs)
                    .iter()
                    .find(|field| field.name(&tx.mod_outline.fields) == field_name)?;

                let ty = field
                    .ty(&tx.mod_outline.fields)
                    .to_ty2(tx.mod_outline, &mut tx.ty_env)
                    .into_ptr(k_mut);
                Some((Some(field), ty))
            }
        }
    };

    trial().unwrap_or_else(|| {
        log::error!(
            "{} left_ty={:?}",
            hint,
            left_ty.display(&tx.ty_env, tx.mod_outline)
        );
        tx.logger.error(loc, "bad type".into());
        (None, KTy2::Never)
    })
}

fn resolve_var_def(term: &mut KVarTerm, expected_ty_opt: Option<&KTy2>, tx: &mut Tx) {
    if term.ty(&tx.local_vars).is_unresolved() {
        let expected_ty = match expected_ty_opt {
            None => fresh_meta_ty(tx),
            Some(ty) => ty.clone(),
        };

        *term.ty_mut(&mut tx.local_vars) = expected_ty;
        return;
    }

    if let Some(expected_ty) = expected_ty_opt {
        let term_ty = term.ty(&tx.local_vars);
        unify2(&term_ty, expected_ty, term.loc(), tx);
    }
}

fn resolve_var_use(term: &mut KVarTerm, tx: &mut Tx) -> KTy2 {
    let current_ty = term.ty(&tx.local_vars);
    if current_ty.is_unresolved() {
        error!("def_ty is unresolved. symbol is undefined? {:?}", term);
    }

    current_ty
}

fn resolve_pat(pat: &mut KTerm, expected_ty: &KTy2, tx: &mut Tx) {
    match pat {
        KTerm::Name(term) => {
            resolve_var_def(term, Some(expected_ty), tx);
        }
        _ => {
            resolve_term(pat, tx);
        }
    }
}

fn resolve_term(term: &mut KTerm, tx: &mut Tx) -> KTy2 {
    match term {
        KTerm::Unit { .. } => KTy2::Unit,
        KTerm::Int { ty, .. } => ty.clone(),
        KTerm::Float { ty, .. } => ty.clone(),
        KTerm::Char { ty, .. } => ty.clone(),
        KTerm::Str { .. } => KTy2::C8.into_ptr(KMut::Const),
        KTerm::True { .. } | KTerm::False { .. } => KTy2::BOOL,
        KTerm::Const { k_const, .. } => k_const
            .ty(&tx.mod_outline.consts)
            .to_ty2(tx.mod_outline, &mut tx.ty_env),
        KTerm::StaticVar { static_var, .. } => static_var
            .ty(&tx.mod_outline.static_vars)
            .to_ty2(tx.mod_outline, &mut tx.ty_env),
        KTerm::Fn { ty, .. } => ty.clone(),
        KTerm::Label { label, .. } => label.ty(&tx.label_sigs),
        KTerm::Return { .. } => tx
            .return_ty_opt
            .as_ref()
            .unwrap()
            .to_ty2(tx.mod_outline, &mut tx.ty_env),
        KTerm::ExternFn { extern_fn, .. } => extern_fn
            .ty(&tx.mod_outline.extern_fns)
            .to_ty2(tx.mod_outline, &mut tx.ty_env),
        KTerm::Name(term) => resolve_var_use(term, tx),
        KTerm::RecordTag { k_struct, .. } => k_struct
            .tag_ty(&tx.mod_outline.structs, &tx.mod_outline.struct_enums)
            .to_ty2(tx.mod_outline, &mut tx.ty_env),
        KTerm::FieldTag(_) => unreachable!(),
        KTerm::TyProperty { kind, ty, loc } => {
            let value_opt = match kind {
                KTyProperty::AlignOf => ty.align_of(&tx.ty_env, &tx.mod_outline),
                KTyProperty::SizeOf => ty.size_of(&tx.ty_env, &tx.mod_outline),
            };
            if value_opt.is_none() {
                error_invalid_size_of(*loc, &tx.logger);
            }
            KTy2::USIZE
        }
    }
}

fn resolve_terms(terms: &mut [KTerm], tx: &mut Tx) -> Vec<KTy2> {
    terms
        .iter_mut()
        .map(|term| resolve_term(term, tx))
        .collect()
}

fn resolve_node(node: &mut KNode, tx: &mut Tx) {
    match node.prim {
        KPrim::Stuck => {}
        KPrim::Jump => match node.args.as_mut_slice() {
            [callee, args @ ..] => loop {
                let label = match *callee {
                    KTerm::Label { label, .. } => label,
                    KTerm::Return { .. } => {
                        let callee_ty = resolve_term(callee, tx);
                        let arg_tys = resolve_terms(args, tx);

                        let (param_tys, _) = callee_ty.as_fn(&tx.ty_env).unwrap();
                        let result_ty = param_tys.first().unwrap();

                        let arg_ty = arg_tys.first().cloned().unwrap_or(KTy2::Unit);
                        unify2(&result_ty, &arg_ty, node.loc, tx);
                        break;
                    }
                    _ => callee.with_debug(
                        tx.mod_outline,
                        Some(&tx.local_vars),
                        Some(&tx.label_sigs),
                        |callee| unreachable!("{:?}", callee),
                    ),
                };

                let param_tys = label.of_mut(&mut tx.label_sigs).param_tys_mut();
                if param_tys.len() > args.len() {
                    tx.logger.error(
                        node.loc,
                        format!("{} 個の引数が必要です。", param_tys.len()),
                    );
                }

                let param_tys = param_tys.clone();
                let mut bounds = tx.take_label_bound(label, param_tys.len());

                let arg_tys = resolve_terms(args, tx);

                for (((param_ty, bound), arg), arg_ty) in
                    param_tys.iter().zip(&mut bounds).zip(args).zip(arg_tys)
                {
                    // 型注釈があるか、シグネチャが確定済みのときは単一化する。
                    if !param_ty.is_unbound(&tx.ty_env) {
                        unify2(param_ty, &arg_ty, arg.loc(), tx);
                        continue;
                    }

                    let current = replace(bound, KTy2::DEFAULT);
                    *bound = current.join(&arg_ty, &tx.ty_env);
                }

                tx.param_bounds.insert(label, bounds);
                break;
            },
            _ => unimplemented!(),
        },
        KPrim::CallDirect => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([callee, args @ ..], [result]) => loop {
                let def_fn_ty = resolve_term(callee, tx);
                let arg_tys = resolve_terms(args, tx);

                let (param_tys, result_ty) = match def_fn_ty.as_fn(&tx.ty_env) {
                    Some(it) => it,
                    None => {
                        tx.logger
                            .error(node.loc, "関数ではないものは呼び出せません".into());
                        break;
                    }
                };

                // FIXME: 引数の個数を検査する

                for (param_ty, arg_ty) in param_tys.iter().zip(&arg_tys) {
                    unify2(param_ty, arg_ty, node.loc, tx);
                }
                resolve_var_def(result, Some(&result_ty), tx);
                break;
            },
            _ => unimplemented!(),
        },
        KPrim::Record => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let k_struct = match ty.as_struct(&tx.ty_env) {
                    Some(it) => it,
                    None => {
                        tx.logger.error(node.loc, "レコード型が必要です。".into());
                        return;
                    }
                };
                for (arg, field) in node
                    .args
                    .iter_mut()
                    .zip(k_struct.fields(&tx.mod_outline.structs))
                {
                    let arg_ty = resolve_term(arg, tx);
                    unify2(
                        &get_field_ty(ty, *field, node.loc, tx),
                        &arg_ty,
                        node.loc,
                        tx,
                    );
                }

                let result_ty = match k_struct.of(&tx.mod_outline.structs).parent {
                    KStructParent::Enum { struct_enum, .. } => KTy2::StructEnum(struct_enum),
                    KStructParent::Struct { .. } => ty.clone(),
                };
                resolve_var_def(result, Some(&result_ty), tx);

                if !ty.is_struct_or_enum(&tx.ty_env) {
                    // FIXME: DebugWithContext を使う
                    tx.logger
                        .error(result.loc(), format!("struct or enum type required"));
                }
            }
            _ => unimplemented!(),
        },
        KPrim::GetField => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, KTerm::FieldTag(field_tag)], [result]) => {
                let left_ty = resolve_term(left, tx);

                let (field_opt, result_ty) = get_field_or_variant(
                    "KPrim::GetField",
                    &field_tag.name,
                    &left_ty,
                    KMut::Const,
                    field_tag.loc,
                    tx,
                );

                field_tag.ty = result_ty.clone();
                field_tag.field_opt = field_opt;

                resolve_var_def(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::GetFieldMut => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, KTerm::FieldTag(field_tag)], [result]) => {
                let loc = field_tag.loc;
                let left_ty = resolve_term(left, tx);

                let (field_opt, result_ty) = get_field_or_variant(
                    "KPrim::GetFieldMut",
                    &field_tag.name,
                    &left_ty,
                    KMut::Mut,
                    loc,
                    tx,
                );

                field_tag.ty = result_ty.clone();
                field_tag.field_opt = field_opt;
                resolve_var_def(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::If => match node.args.as_mut_slice() {
            [cond] => {
                let cond_ty = resolve_term(cond, tx);
                unify2(&KTy2::BOOL, &cond_ty, node.loc, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Switch => match node.args.as_mut_slice() {
            [cond, pats @ ..] => {
                let cond_ty = resolve_term(cond, tx);

                for pat in pats {
                    resolve_pat(pat, &cond_ty, tx);
                }
            }
            _ => unimplemented!(),
        },
        KPrim::Let => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([init], [result]) => {
                let init_ty = resolve_term(init, tx);

                // 型注釈と単一化する。
                let result_ty = {
                    let expected_ty = result.ty(&tx.local_vars);
                    if expected_ty.is_unresolved() {
                        init_ty
                    } else {
                        unify2(&expected_ty, &init_ty, node.loc, tx);
                        expected_ty
                    }
                };

                resolve_var_def(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);

                let result_ty_opt = arg_ty.as_ptr(&tx.ty_env).map(|(_, ty)| ty);
                if result_ty_opt.is_none() {
                    tx.logger.error(result.loc(), "expected a reference".into());
                }
                resolve_var_def(result, result_ty_opt.as_ref(), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Ref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_var_def(result, Some(&arg_ty.into_ptr(KMut::Const)), tx);
            }
            _ => unimplemented!(),
        },
        // FIXME: ref の実装と重複
        KPrim::RefMut => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_var_def(result, Some(&arg_ty.into_ptr(KMut::Mut)), tx);
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
                let target_ty = take(ty);
                resolve_var_def(result, Some(&target_ty), tx);

                // FIXME: 同じ型へのキャストは警告?
                if let KTy2::Unresolved { .. } | KTy2::Never = arg_ty {
                    // Skip.
                } else if !arg_ty.is_primitive(&tx.ty_env) {
                    // FIXME: DebugWithContext を使う
                    tx.logger
                        .error(node.loc, format!("can't cast from non-primitive type"));
                } else if !target_ty.is_primitive(&tx.ty_env) {
                    // FIXME: DebugWithContext を使う
                    let msg = format!("can't cast to non-primitive type");
                    tx.logger.error(node.loc, msg);
                }
            }
            _ => unimplemented!(),
        },
        KPrim::Minus => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                // FIXME: bool or iNN
                resolve_var_def(result, Some(&arg_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Not => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                // FIXME: bool or iNN or uNN
                resolve_var_def(result, Some(&arg_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Add | KPrim::Sub => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([left, right], [result]) => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);

                if left_ty.is_ptr(&tx.ty_env) {
                    unify2(&KTy2::USIZE, &right_ty, node.loc, tx);
                } else {
                    // FIXME: iNN or uNN
                    unify2(&left_ty, &right_ty, node.loc, tx);
                }

                resolve_var_def(result, Some(&left_ty), tx);
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
                unify2(&left_ty, &right_ty, node.loc, tx);

                resolve_var_def(result, Some(&left_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Equal
        | KPrim::NotEqual
        | KPrim::LessThan
        | KPrim::LessEqual
        | KPrim::GreaterThan
        | KPrim::GreaterEqual => {
            match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
                ([left, right], [result]) => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);
                    // FIXME: Eq/Ord only
                    unify2(&left_ty, &right_ty, node.loc, tx);

                    resolve_var_def(result, Some(&KTy2::BOOL), tx);
                }
                _ => unimplemented!(),
            }
        }
        KPrim::Assign => match node.args.as_mut_slice() {
            [left, right] => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);

                // 左辺は lval なのでポインタ型のはず
                let left_ty = match left_ty.as_ptr(&tx.ty_env) {
                    Some((KMut::Mut, left_ty)) => left_ty,
                    Some((KMut::Const, left_ty)) => {
                        tx.logger
                            .error(left.loc(), "expected mutable reference".into());
                        left_ty
                    }
                    None => KTy2::Never,
                };

                unify2(&left_ty, &right_ty, node.loc, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::AddAssign | KPrim::SubAssign => {
            match node.args.as_mut_slice() {
                [left, right] => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);

                    // 左辺は lval なのでポインタ型のはず。ただし不正なコードだと unresolved/never になることもある。
                    let (k_mut, left_ty) = match left_ty.as_ptr(&tx.ty_env) {
                        Some(it) => it,
                        None => {
                            log::error!(
                                "AddAssign etc. left={}",
                                left_ty.display(&tx.ty_env, &tx.mod_outline)
                            );
                            (
                                KMut::Mut,
                                KTy2::Unresolved {
                                    cause: KTyCause::Loc(node.loc),
                                },
                            )
                        }
                    };
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(left.loc(), "unexpected const reference".into());
                    }

                    // FIXME: add/sub と同じ
                    if left_ty.is_ptr(&tx.ty_env) {
                        unify2(&KTy2::USIZE, &right_ty, node.loc, tx);
                    } else {
                        // FIXME: iNN or uNN
                        unify2(&left_ty, &right_ty, node.loc, tx);
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
                    let (k_mut, left_ty) = match left_ty.as_ptr(&tx.ty_env) {
                        Some(it) => it,
                        None => {
                            log::error!(
                                "MulAssign etc. left={}",
                                left_ty.display(&tx.ty_env, &tx.mod_outline)
                            );
                            (
                                KMut::Mut,
                                KTy2::Unresolved {
                                    cause: KTyCause::Loc(node.loc),
                                },
                            )
                        }
                    };
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(left.loc(), "unexpected const reference".into());
                    }

                    // FIXME: mul/div/etc. と同じ
                    // FIXME: iNN or uNN
                    unify2(&left_ty, &right_ty, node.loc, tx);
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
    assert!(tx.ty_env.is_empty());
    swap(&mut tx.ty_env, &mut fn_data.ty_env);

    for i in 0..fn_data.params.len() {
        let param = &mut fn_data.params[i];
        let param_ty = &k_fn.param_tys(&tx.mod_outline.fns)[i].to_ty2_poly(tx.mod_outline);
        resolve_var_def(param, Some(param_ty), tx);
    }

    // いまから生成するところなので空のはず。
    assert!(fn_data.label_sigs.is_empty());

    for label_data in fn_data.labels.iter_mut() {
        for param in &mut label_data.params {
            resolve_var_def(param, None, tx);
        }

        let label_sig = {
            let name = label_data.name.to_string();
            let param_tys = label_data
                .params
                .iter()
                .map(|param| param.ty(&tx.local_vars))
                .collect();
            KLabelSig::new(name, param_tys)
        };
        fn_data.label_sigs.alloc(label_sig);
    }

    assert_eq!(fn_data.label_sigs.len(), fn_data.labels.len());

    swap(&mut tx.ty_env, &mut fn_data.ty_env);
}

fn prepare_extern_fn(extern_fn: KExternFn, data: &mut KExternFnData, tx: &mut Tx) {
    for i in 0..data.params.len() {
        let param = &mut data.params[i];
        let param_ty =
            &extern_fn.param_tys(&tx.mod_outline.extern_fns)[i].to_ty2_poly(tx.mod_outline);
        resolve_var_def(param, Some(&param_ty), tx);
    }
}

fn resolve_root(root: &mut KModData, tx: &mut Tx) {
    for (extern_fn, extern_fn_data) in root.extern_fns.enumerate_mut() {
        swap(&mut tx.local_vars, &mut extern_fn_data.local_vars);

        prepare_extern_fn(extern_fn, extern_fn_data, tx);

        swap(&mut tx.local_vars, &mut extern_fn_data.local_vars);
    }

    // HACK: label_sigs が空でない関数はすでに型検査済みなので無視する。応急処置
    for (k_fn, fn_data) in root
        .fns
        .enumerate_mut()
        .filter(|(_, fn_data)| fn_data.label_sigs.is_empty())
    {
        swap(&mut tx.local_vars, &mut fn_data.local_vars);

        prepare_fn(k_fn, fn_data, tx);

        swap(&mut tx.local_vars, &mut fn_data.local_vars);
    }

    // 項の型を解決する。
    for (k_fn, fn_data) in root.fns.enumerate_mut() {
        #[cfg(skip)]
        log::trace!("type res fn {}", k_fn.of(&tx.mod_outline.fns).name);

        tx.return_ty_opt = Some(k_fn.return_ty(&tx.mod_outline.fns));
        swap(&mut tx.local_vars, &mut fn_data.local_vars);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);

        for (label, label_data) in fn_data.labels.enumerate_mut() {
            #[cfg(skip)]
            log::trace!("type res label {}", label_data.name);

            let bounds = tx.take_label_bound(label, label_data.params.len());
            for (bound, param) in bounds.iter().zip(label_data.params.iter()) {
                unify2(&bound, &param.ty(&tx.local_vars), param.loc(), tx);
            }

            resolve_node(&mut label_data.body, tx);
        }

        for local_var_data in tx.local_vars.iter_mut() {
            if local_var_data.ty.is_unbound(&tx.ty_env) {
                local_var_data.ty = KTy2::Never;
            }
        }

        swap(&mut tx.local_vars, &mut fn_data.local_vars);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);
        tx.return_ty_opt.take();
    }
}

pub(crate) fn resolve_types(mod_outline: &KModOutline, mod_data: &mut KModData, logger: Logger) {
    KModOutline::given_for_debug(mod_outline, || {
        let mut tx = Tx::new(mod_outline, logger);
        resolve_root(mod_data, &mut tx);
    });
}
