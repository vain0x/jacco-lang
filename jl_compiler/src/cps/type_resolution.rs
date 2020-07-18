//! 型推論・型検査

use super::*;
use k_mod::KModLocalSymbolOutline;
use k_ty::{KEnumOrStruct, KTy2, Variance};
use std::mem::{replace, swap, take};

/// Typing context. 型検査の状態
struct Tx<'a> {
    k_mod: KMod,
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
    fn new(
        k_mod: KMod,
        outlines: &'a KModOutline,
        mod_outlines: &'a KModOutlines,
        logger: Logger,
    ) -> Self {
        Self {
            ty_env: KTyEnv::default(),
            locals: Default::default(),
            label_sigs: Default::default(),
            return_ty_opt: None,
            k_mod,
            outlines,
            mod_outlines,
            logger,
        }
    }
}

struct UnificationContext<'a> {
    variance: Variance,
    location: Location,
    ty_env: &'a KTyEnv,
    logger: &'a Logger,
}

impl<'a> UnificationContext<'a> {
    fn new(location: Location, ty_env: &'a KTyEnv, logger: &'a Logger) -> Self {
        Self {
            variance: Variance::Co,
            location,
            ty_env,
            logger,
        }
    }

    fn do_with_contra_variant(&mut self, f: impl FnOnce(&mut Self)) {
        self.variance.reverse();
        f(self);
        self.variance.reverse();
    }

    fn do_with_invariant(&mut self, f: impl FnOnce(&mut Self)) {
        let variance = replace(&mut self.variance, Variance::In);
        f(self);
        self.variance = variance;
    }

    fn bug_unresolved(&self, other: &KTy2) {
        log::error!(
            "unresolved な型は、単一化中に束縛できるように、型検査の前にメタ型変数に置き換えておく必要があります (other={:?}, location={:?})",
            other.display(&self.ty_env),
            self.location
        );
    }

    fn error_ptr_mut(&self) {
        self.logger
            .error(self.location, "ポインタの可変性に互換性がありません");
    }

    fn error_arity(&self, left: &KTy2, right: &KTy2) {
        let ty_env = &self.ty_env;
        // FIXME: エラーメッセージを改善
        self.logger.error(
            self.location,
            format!(
                "型引数の個数が一致しません {:?}",
                (left.display(ty_env), right.display(ty_env))
            ),
        );
    }

    fn error_ununifiable(&self, left: &KTy2, right: &KTy2) {
        let ty_env = &self.ty_env;
        self.logger.error(
            self.location,
            format!(
                "型が一致しません ({} <- {})",
                left.display(ty_env),
                right.display(ty_env)
            ),
        );
    }
}

fn do_unify2(left: &KTy2, right: &KTy2, ux: &mut UnificationContext<'_>) {
    match (left, right) {
        (KTy2::Unresolved, other) | (other, KTy2::Unresolved) => {
            ux.bug_unresolved(other);
        }

        // never は任意の型に upcast できる。
        (_, KTy2::Never) if ux.variance == Variance::Co => {}
        (KTy2::Never, _) if ux.variance == Variance::Contra => {}

        // 左右の型が完全に一致するケース:
        (KTy2::Meta(..), KTy2::Meta(..))
        | (KTy2::Basic(..), KTy2::Basic(..))
        | (KTy2::Enum(..), KTy2::Enum(..))
        | (KTy2::Struct(..), KTy2::Struct(..))
            if left == right => {}

        // メタ型変数の展開および束縛:
        (_, KTy2::Meta(meta_ty)) => match meta_ty.try_unwrap(&ux.ty_env) {
            Some(ty_cell) => do_unify2(left, &ty_cell.borrow(), ux),
            None => {
                // fn main() -> i32 { loop {} } などで発生する。
                // この時点で unbound な型変数は never とみなしてよいはず?
                do_unify2(left, &KTy2::Never, ux)
            }
        },
        (KTy2::Meta(meta_ty), _) => {
            match meta_ty.try_unwrap(&ux.ty_env) {
                Some(ty_cell) => do_unify2(&ty_cell.borrow(), right, ux),
                None => {
                    // FIXME: occurrence check
                    meta_ty.bind(right.clone(), &ux.ty_env);
                }
            }
        }

        // その他、型ごとのルール:
        (
            KTy2::Ptr {
                k_mut,
                base_ty: left,
            },
            KTy2::Ptr {
                k_mut: right_mut,
                base_ty: right,
            },
        ) => {
            let ok = match ux.variance {
                Variance::In => k_mut == right_mut,
                Variance::Co => k_mut <= right_mut,
                Variance::Contra => k_mut >= right_mut,
            };
            if !ok {
                ux.error_ptr_mut();
                return;
            }

            match k_mut {
                KMut::Const => do_unify2(left, right, ux),
                KMut::Mut => ux.do_with_invariant(|ux| do_unify2(left, right, ux)),
            }
        }
        (
            KTy2::Fn {
                param_tys,
                result_ty,
            },
            KTy2::Fn {
                param_tys: right_param_tys,
                result_ty: right_result_ty,
            },
        ) => {
            if param_tys.len() != right_param_tys.len() {
                ux.error_arity(left, right);
                return;
            }

            ux.do_with_contra_variant(|ux| {
                for (left, right) in param_tys.iter().zip(right_param_tys) {
                    do_unify2(left, right, ux);
                }
            });

            do_unify2(result_ty, right_result_ty, ux);
        }

        // 不一致
        (KTy2::Never, _)
        | (KTy2::Basic(_), _)
        | (KTy2::Ptr { .. }, _)
        | (KTy2::Fn { .. }, _)
        | (KTy2::Enum(..), _)
        | (KTy2::Struct(..), _) => {
            ux.error_ununifiable(left, right);
        }
    }
}

/// left, right の型が一致するように型変数を決定する。
/// (right 型の値を left 型の引数に代入する状況を想定する。)
fn unify(left: KTy, right: KTy, location: Location, tx: &mut Tx) {
    let mut ux = UnificationContext::new(location, &tx.ty_env, &tx.logger);
    do_unify2(&left.to_ty2(tx.k_mod), &right.to_ty2(tx.k_mod), &mut ux);
}

/// left 型の変数に right 型の値を代入できるか判定する。
/// 必要に応じて型変数を束縛する。
fn unify2(left: &KTy2, right: &KTy2, location: Location, tx: &mut Tx) {
    let mut ux = UnificationContext::new(location, &tx.ty_env, &tx.logger);
    do_unify2(&left, &right, &mut ux);
}

fn fresh_meta_ty(location: Location, tx: &mut Tx) -> KTy2 {
    let meta_ty = tx.ty_env.meta_ty_new(location);
    KTy2::Meta(meta_ty)
}

fn resolve_symbol_def2(symbol: &mut KSymbol, expected_ty_opt: Option<&KTy2>, tx: &mut Tx) {
    if symbol.ty(&tx.locals).is_unresolved() {
        let expected_ty = match expected_ty_opt {
            None => fresh_meta_ty(symbol.location(), tx),
            Some(ty) => ty.clone(),
        };

        *symbol.ty_mut(&mut tx.locals) = expected_ty;
        return;
    }

    if let Some(expected_ty) = expected_ty_opt {
        let symbol_ty = symbol.ty(&tx.locals);
        unify2(&symbol_ty, expected_ty, symbol.location, tx);
    }
}

fn resolve_symbol_def(symbol: &mut KSymbol, expected_ty_opt: Option<&KTy>, tx: &mut Tx) {
    let expected_ty_opt = expected_ty_opt.map(|ty| ty.to_ty2(tx.k_mod));
    resolve_symbol_def2(symbol, expected_ty_opt.as_ref(), tx)
}

fn resolve_symbol_use(symbol: &mut KSymbol, tx: &mut Tx) -> KTy {
    let current_ty = symbol.ty(&tx.locals);
    if current_ty.is_unresolved() {
        error!("def_ty is unresolved. symbol is undefined? {:?}", symbol);
    }

    current_ty.to_ty1()
}

fn resolve_pat(pat: &mut KTerm, expected_ty: &KTy, tx: &mut Tx) -> KTy {
    match pat {
        KTerm::Name(symbol) => {
            resolve_symbol_def(symbol, Some(expected_ty), tx);
            symbol.ty(&tx.locals).to_ty1()
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
            assert!(ty.is_primitive(&tx.ty_env));
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
        KTerm::Label { label, .. } => label.ty(&tx.label_sigs).to_ty1(),
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

                let (param_tys, _) = def_fn_ty.as_fn(&tx.ty_env).unwrap();

                // FIXME: 引数の個数を検査する

                for (param_ty, arg_ty) in param_tys.iter().zip(arg_tys) {
                    unify(param_ty.clone(), arg_ty.clone(), node.location, tx);
                }
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([callee, args @ ..], [result]) => {
                let def_fn_ty = resolve_term(callee, tx);
                let arg_tys = resolve_terms(args, tx);

                let (param_tys, result_ty) = def_fn_ty.as_fn(&tx.ty_env).unwrap();

                // FIXME: 引数の個数を検査する

                for (param_ty, arg_ty) in param_tys.iter().zip(arg_tys) {
                    unify(param_ty.clone(), arg_ty.clone(), node.location, tx);
                }
                resolve_symbol_def(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Record => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let k_struct = ty.clone().as_struct(&tx.ty_env).unwrap();
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

                if !ty.is_struct_or_enum(&tx.ty_env) {
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
                    let (_, ty) = left_ty.as_ptr(&tx.ty_env)?;
                    let ty = match ty.as_struct_or_enum(&tx.ty_env)? {
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
                    let (k_mut, ty) = left_ty.as_ptr(&tx.ty_env)?;
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(&left.location(tx.outlines), "unexpected const reference");
                    }

                    let ty = match ty.as_struct_or_enum(&tx.ty_env)? {
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

                let result_ty_opt = arg_ty.as_ptr(&tx.ty_env).map(|(_, ty)| ty);
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
                } else if !arg_ty.is_primitive(&tx.ty_env) {
                    tx.logger.error(
                        &node,
                        format!("can't cast from non-primitive type {:?}", arg_ty),
                    );
                } else if !ty.is_primitive(&tx.ty_env) {
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

                if left_ty.is_ptr(&tx.ty_env) {
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
                let left_ty = match left_ty.as_ptr(&tx.ty_env) {
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
                    let (k_mut, left_ty) = left_ty.as_ptr(&tx.ty_env).unwrap();
                    if let KMut::Const = k_mut {
                        tx.logger
                            .error(&left.location(tx.outlines), "unexpected const reference");
                    }

                    // FIXME: add/sub と同じ
                    if left_ty.is_ptr(&tx.ty_env) {
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
                    let (k_mut, left_ty) = left_ty.as_ptr(&tx.ty_env).unwrap();
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
            if local_data.ty.is_unbound(&tx.ty_env) {
                local_data.ty = KTy2::Never;
            }
        }

        swap(&mut tx.locals, &mut fn_data.locals);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);
        tx.return_ty_opt.take();
    }
}

pub(crate) fn resolve_types(
    k_mod: KMod,
    outlines: &KModOutline,
    mod_data: &mut KModData,
    mod_outlines: &KModOutlines,
    logger: Logger,
) {
    let mut tx = Tx::new(k_mod, outlines, mod_outlines, logger);
    resolve_root(mod_data, &mut tx);
}
