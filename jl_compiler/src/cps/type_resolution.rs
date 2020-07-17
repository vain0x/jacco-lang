//! 型推論・型検査

use super::*;
use k_mod::KModLocalSymbolOutline;
use k_ty_env::KEnumOrStruct;
use std::mem::{swap, take};

/// Typing context. 型検査の状態
struct Tx<'a> {
    /// 現在の関数の型環境
    ty_env: KTyEnv,
    /// 現在の関数に含まれるローカル変数の情報
    locals: KLocalArena,
    /// 現在の関数に含まれるラベルのシグネチャ情報
    label_sigs: KLabelSigArena,
    /// 現在の関数の return ラベルの型
    return_ty_opt: Option<KTy>,
    /// 検査対象のモジュールのアウトライン
    outlines: &'a KModOutline,
    /// プロジェクト内のモジュールのアウトライン
    mod_outlines: &'a KModOutlines,
    logger: Logger,
}

impl<'a> Tx<'a> {
    fn new(outlines: &'a KModOutline, mod_outlines: &'a KModOutlines, logger: Logger) -> Self {
        Self {
            ty_env: KTyEnv::default(),
            locals: Default::default(),
            label_sigs: Default::default(),
            return_ty_opt: None,
            outlines,
            mod_outlines,
            logger,
        }
    }

    fn fresh_meta_ty(&mut self, location: Location) -> KTy {
        let meta_ty = self.ty_env.meta_ty_new(location);
        KTy::Meta(meta_ty)
    }
}

fn do_unify(left: &KTy, right: &KTy, location: Location, tx: &Tx) {
    match (left, right) {
        (KTy::Never, _) | (_, KTy::Never) => {}

        (KTy::Unresolved, other) | (other, KTy::Unresolved) => {
            log::error!(
                "don't try to unify unresolved meta tys (other={:?}, location={:?})",
                other,
                location
            );
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
        | (KTy::I8, KTy::I8)
        | (KTy::I16, KTy::I16)
        | (KTy::I32, KTy::I32)
        | (KTy::I64, KTy::I64)
        | (KTy::Isize, KTy::Isize)
        | (KTy::U8, KTy::U8)
        | (KTy::U16, KTy::U16)
        | (KTy::U32, KTy::U32)
        | (KTy::U64, KTy::U64)
        | (KTy::Usize, KTy::Usize)
        | (KTy::F32, KTy::F32)
        | (KTy::F64, KTy::F64)
        | (KTy::C8, KTy::C8)
        | (KTy::C16, KTy::C16)
        | (KTy::C32, KTy::C32)
        | (KTy::Bool, KTy::Bool) => {}

        (
            KTy::Ptr {
                k_mut: left_mut,
                ty: left,
            },
            KTy::Ptr {
                k_mut: right_mut,
                ty: right,
            },
        ) => {
            do_unify(left, right, location, tx);

            if let (KMut::Mut, KMut::Const) = (left_mut, right_mut) {
                tx.logger
                    .error(location, "cannot convert const ptr to mut ptr");
            }
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

        (KTy::Enum(left), KTy::Enum(right)) if left == right => {}
        (KTy::Struct(left), KTy::Struct(right)) if left == right => {}

        (KTy::Unit, _)
        | (KTy::I8, _)
        | (KTy::I16, _)
        | (KTy::I32, _)
        | (KTy::I64, _)
        | (KTy::Isize, _)
        | (KTy::U8, _)
        | (KTy::U16, _)
        | (KTy::U32, _)
        | (KTy::U64, _)
        | (KTy::Usize, _)
        | (KTy::F32, _)
        | (KTy::F64, _)
        | (KTy::C8, _)
        | (KTy::C16, _)
        | (KTy::C32, _)
        | (KTy::Bool, _)
        | (KTy::Ptr { .. }, _)
        | (KTy::Fn { .. }, _)
        | (KTy::Enum { .. }, _)
        | (KTy::Struct { .. }, _) => {
            tx.logger.error(
                location,
                format!("type mismatch ({:?} vs. {:?})", left, right),
            );
        }
    }
}

/// left, right の型が一致するように型変数を決定する。
/// (right 型の値を left 型の引数に代入する状況を想定する。)
fn unify(left: KTy, right: KTy, location: Location, tx: &mut Tx) {
    do_unify(&left, &right, location, tx);
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
        do_unify(&symbol_ty, expected_ty, symbol.location, tx);
    }
}

fn resolve_symbol_use(symbol: &mut KSymbol, tx: &mut Tx) -> KTy {
    let current_ty = symbol.ty(&tx.locals);
    if current_ty.is_unresolved() {
        error!("def_ty is unresolved. symbol is undefined? {:?}", symbol);
    }

    current_ty
}

fn resolve_pat(pat: &mut KTerm, expected_ty: &KTy, tx: &mut Tx) -> KTy {
    match pat {
        KTerm::Name(symbol) => {
            resolve_symbol_def(symbol, Some(expected_ty), tx);
            symbol.ty(&tx.locals)
        }
        _ => resolve_term(pat, tx),
    }
}

fn resolve_alias_term(alias: KAlias, location: Location, tx: &mut Tx) -> KTy {
    let outline = match alias
        .of(&tx.outlines.aliases)
        .referent_outline(&tx.mod_outlines)
    {
        Some(outline) => outline,
        None => {
            tx.logger.error(
                location,
                "解決されていないエイリアスの型が {unresolved} になりました",
            );
            return KTy::Unresolved;
        }
    };

    match outline {
        k_mod::KProjectSymbolOutline::Mod(..) => {
            tx.logger.error(
                location,
                "モジュールを指すエイリアスを値として使うことはできません",
            );
            KTy::Never
        }
        k_mod::KProjectSymbolOutline::ModLocal { symbol_outline, .. } => match symbol_outline {
            KModLocalSymbolOutline::Alias(alias, alias_data) => {
                resolve_alias_term(alias, alias_data.location(), tx)
            }
            KModLocalSymbolOutline::Const(_, const_data) => const_data.value_ty.clone(),
            KModLocalSymbolOutline::StaticVar(_, static_var_outline) => {
                static_var_outline.ty.clone()
            }
            KModLocalSymbolOutline::Fn(_, fn_outline) => fn_outline.ty(),
            KModLocalSymbolOutline::ExternFn(_, extern_fn_outline) => extern_fn_outline.ty(),
            KModLocalSymbolOutline::Enum(_, _) | KModLocalSymbolOutline::Struct(_, _) => {
                tx.logger
                    .unimpl(location, "インポートされた型の型検査は未実装です");
                KTy::Never
            }
            KModLocalSymbolOutline::LocalVar(..) => {
                tx.logger
                    .unexpected(location, "エイリアスがローカル変数を指すことはありません");
                KTy::Never
            }
        },
    }
}

fn resolve_term(term: &mut KTerm, tx: &mut Tx) -> KTy {
    match term {
        KTerm::Unit { .. } => KTy::Unit,
        KTerm::Int(token, ty) => {
            // FIXME: i8 などに対応
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
        KTerm::Str(_) => KTy::C8.into_ptr(KMut::Const),
        KTerm::True(_) | KTerm::False(_) => KTy::Bool,
        KTerm::Alias { alias, location } => resolve_alias_term(*alias, *location, tx),
        KTerm::Const { k_const, .. } => k_const.ty(&tx.outlines.consts),
        KTerm::StaticVar { static_var, .. } => static_var.ty(&tx.outlines.static_vars).clone(),
        KTerm::Fn { k_fn, .. } => k_fn.ty(&tx.outlines.fns),
        KTerm::Label { label, .. } => label.ty(&tx.label_sigs),
        KTerm::Return { .. } => tx.return_ty_opt.clone().unwrap(),
        KTerm::ExternFn { extern_fn, .. } => extern_fn.ty(&tx.outlines.extern_fns),
        KTerm::Name(symbol) => resolve_symbol_use(symbol, tx),
        KTerm::RecordTag { k_struct, .. } => k_struct
            .tag_ty(&tx.outlines.structs, &tx.outlines.enums)
            .clone(),
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

                unify(def_fn_ty, use_fn_ty, node.location, tx);
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

                unify(def_fn_ty, use_fn_ty, node.location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::EnumToTag => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);

                let result_ty = match tx.ty_env.as_enum(&arg_ty) {
                    Some(k_enum) => k_enum.tag_ty(&tx.outlines.enums).clone(),
                    None => {
                        error!("expected enum type {:?}", arg);
                        KTy::Never
                    }
                };

                resolve_symbol_def(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Record => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let k_struct = ty.clone().as_struct().unwrap();
                let outlines = tx.outlines.clone();

                for (arg, field) in node.args.iter_mut().zip(k_struct.fields(&outlines.structs)) {
                    let arg_ty = resolve_term(arg, tx);
                    unify(
                        field.ty(&tx.outlines.fields).clone(),
                        arg_ty,
                        node.location,
                        tx,
                    );
                }

                let ty = k_struct.ty(&outlines.structs);
                resolve_symbol_def(result, Some(&ty), tx);

                if !tx.ty_env.is_struct_or_enum(&ty) {
                    tx.logger
                        .error(result, format!("struct or enum type required {:?}", ty));
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
                    let (_, ty) = tx.ty_env.as_ptr(&left_ty)?;
                    let ty = match tx.ty_env.as_struct_or_enum(&ty)? {
                        KEnumOrStruct::Enum(k_enum) => k_enum
                            .variants(&tx.outlines.enums)
                            .iter()
                            .find_map(|k_variant| {
                            k_variant
                                .as_record_with_name(&field_name, &tx.outlines.structs)
                                .map(KTy::Struct)
                        })?,
                        KEnumOrStruct::Struct(k_struct) => k_struct
                            .fields(&tx.outlines.structs)
                            .iter()
                            .find(|field| field.name(&tx.outlines.fields) == *field_name)
                            .map(|field| field.ty(&tx.outlines.fields).clone())?,
                    };
                    Some(ty.into_ptr(KMut::Const))
                })()
                .unwrap_or_else(|| {
                    tx.logger.error(location, "bad type");
                    KTy::Never
                });

                resolve_symbol_def(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::GetFieldMut => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            (
                [left, KTerm::FieldTag(KFieldTag {
                    name: field_name,
                    location,
                })],
                [result],
            ) => {
                let left_ty = resolve_term(left, tx);

                let result_ty = (|| {
                    let (k_mut, ty) = tx.ty_env.as_ptr(&left_ty)?;
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(&left.location(tx.outlines), "unexpected const reference");
                    }

                    let ty = match tx.ty_env.as_struct_or_enum(&ty)? {
                        KEnumOrStruct::Enum(k_enum) => k_enum
                            .variants(&tx.outlines.enums)
                            .iter()
                            .find_map(|k_variant| {
                            k_variant
                                .as_record_with_name(&field_name, &tx.outlines.structs)
                                .map(KTy::Struct)
                        })?,
                        KEnumOrStruct::Struct(k_struct) => k_struct
                            .fields(&tx.outlines.structs)
                            .iter()
                            .find(|field| field.name(&tx.outlines.fields) == *field_name)
                            .map(|field| field.ty(&tx.outlines.fields).clone())?,
                    };
                    Some(ty.into_ptr(k_mut))
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
                unify(KTy::Bool, cond_ty, node.location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Switch => match node.args.as_mut_slice() {
            [cond, pats @ ..] => {
                let cond_ty = resolve_term(cond, tx);

                for pat in pats {
                    let _pat_ty = resolve_pat(pat, &cond_ty, tx);
                }
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

                let result_ty_opt = tx.ty_env.as_ptr(&arg_ty).map(|(_, ty)| ty);
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
                resolve_symbol_def(result, Some(&arg_ty.into_ptr(KMut::Const)), tx);
            }
            _ => unimplemented!(),
        },
        // FIXME: ref の実装と重複
        KPrim::RefMut => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_symbol_def(result, Some(&arg_ty.into_ptr(KMut::Mut)), tx);
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
                } else if !tx.ty_env.is_primitive(&arg_ty) {
                    tx.logger.error(
                        &node,
                        format!("can't cast from non-primitive type {:?}", arg_ty),
                    );
                } else if !tx.ty_env.is_primitive(ty) {
                    let msg = format!("can't cast to non-primitive type {:?}", ty);
                    tx.logger.error(&node, msg);
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
                    unify(KTy::Usize, right_ty.clone(), node.location, tx);
                } else {
                    // FIXME: iNN or uNN
                    unify(left_ty.clone(), right_ty, node.location, tx);
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
                unify(left_ty.clone(), right_ty, node.location, tx);

                resolve_symbol_def(result, Some(&left_ty), tx);
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
                    unify(left_ty, right_ty, node.location, tx);

                    resolve_symbol_def(result, Some(&KTy::Bool), tx);
                }
                _ => unimplemented!(),
            }
        }
        KPrim::Assign => match node.args.as_mut_slice() {
            [left, right] => {
                let left_ty = resolve_term(left, tx);
                let right_ty = resolve_term(right, tx);

                // 左辺は lval なのでポインタ型のはず
                let left_ty = match tx.ty_env.as_ptr(&left_ty) {
                    Some((KMut::Mut, left_ty)) => left_ty,
                    Some((KMut::Const, left_ty)) => {
                        tx.logger
                            .error(&left.location(tx.outlines), "expected mutable reference");
                        left_ty
                    }
                    None => KTy::Never,
                };

                unify(left_ty, right_ty, node.location, tx);
            }
            _ => unimplemented!(),
        },
        KPrim::AddAssign | KPrim::SubAssign => {
            match node.args.as_mut_slice() {
                [left, right] => {
                    let left_ty = resolve_term(left, tx);
                    let right_ty = resolve_term(right, tx);

                    // 左辺は lval なのでポインタ型のはず
                    let (k_mut, left_ty) = tx.ty_env.as_ptr(&left_ty).unwrap();
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(&left.location(tx.outlines), "unexpected const reference");
                    }

                    // FIXME: add/sub と同じ
                    if tx.ty_env.is_ptr(&left_ty) {
                        unify(KTy::Usize, right_ty.clone(), node.location, tx);
                    } else {
                        // FIXME: iNN or uNN
                        unify(left_ty.clone(), right_ty, node.location, tx);
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
                    let (k_mut, left_ty) = tx.ty_env.as_ptr(&left_ty).unwrap();
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(&left.location(tx.outlines), "unexpected const reference");
                    }

                    // FIXME: mul/div/etc. と同じ
                    // FIXME: iNN or uNN
                    unify(left_ty.clone(), right_ty, node.location, tx);
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
        let param_ty = &k_fn.param_tys(&outlines.fns)[i];
        resolve_symbol_def(param, Some(param_ty), tx);
    }

    // いまから生成するところなので空のはず。
    assert!(fn_data.label_sigs.is_empty());

    for label_data in fn_data.labels.iter_mut() {
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
        fn_data.label_sigs.alloc(label_sig);
    }

    assert_eq!(fn_data.label_sigs.len(), fn_data.labels.len());

    fn_data.ty_env = take(&mut tx.ty_env);
}

fn prepare_extern_fn(extern_fn: KExternFn, data: &mut KExternFnData, tx: &mut Tx) {
    let outlines = tx.outlines.clone();

    for i in 0..data.params.len() {
        let param = &mut data.params[i];
        let param_ty = &extern_fn.param_tys(&outlines.extern_fns)[i];
        resolve_symbol_def(param, Some(param_ty), tx);
    }
}

fn prepare_struct(k_struct: KStruct, tx: &mut Tx) {
    for field in k_struct.fields(&tx.outlines.structs) {
        if field.ty(&tx.outlines.fields).is_unresolved() {
            // FIXME: handle correctly. unresolved type crashes on unification for now
            tx.logger.error(
                &field.location(&tx.outlines.fields),
                "unresolved field type",
            );
        }
    }
}

fn resolve_root(root: &mut KModData, tx: &mut Tx) {
    let outlines = tx.outlines;

    for k_struct in outlines.structs.keys() {
        prepare_struct(k_struct, tx);
    }

    for (extern_fn, extern_fn_data) in root.extern_fns.enumerate_mut() {
        swap(&mut tx.locals, &mut extern_fn_data.locals);

        prepare_extern_fn(extern_fn, extern_fn_data, tx);

        swap(&mut tx.locals, &mut extern_fn_data.locals);
    }

    for (k_fn, fn_data) in root.fns.enumerate_mut() {
        swap(&mut tx.locals, &mut fn_data.locals);

        prepare_fn(k_fn, fn_data, tx);

        swap(&mut tx.locals, &mut fn_data.locals);
    }

    // 項の型を解決する。
    for (k_fn, fn_data) in root.fns.enumerate_mut() {
        tx.return_ty_opt = Some(k_fn.return_ty(&outlines.fns));
        swap(&mut tx.locals, &mut fn_data.locals);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);

        resolve_node(&mut fn_data.body, tx);

        for label in fn_data.labels.iter_mut() {
            resolve_node(&mut label.body, tx);
        }

        for local_data in tx.locals.iter_mut() {
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

pub(crate) fn resolve_types(
    outlines: &KModOutline,
    mod_data: &mut KModData,
    mod_outlines: &KModOutlines,
    logger: Logger,
) {
    let mut tx = Tx::new(outlines, mod_outlines, logger);
    resolve_root(mod_data, &mut tx);
}
