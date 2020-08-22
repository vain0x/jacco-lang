//! 型推論・型検査

use super::*;
use crate::{source::HaveLoc, source::Loc};
use k_meta_ty::KMetaTyData;
use k_mod::{KModLocalSymbolOutline, KProjectSymbolOutline};
use k_ty::{KEnumOrStruct, KTy2, KTyCause, Variance};
use std::{
    cell::RefCell,
    mem::{replace, swap, take},
};

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
    mod_outline: &'a KModOutline,
    /// プロジェクト内のモジュールのアウトライン
    mod_outlines: &'a KModOutlines,
    logger: Logger,
}

impl<'a> Tx<'a> {
    fn new(
        k_mod: KMod,
        mod_outline: &'a KModOutline,
        mod_outlines: &'a KModOutlines,
        logger: Logger,
    ) -> Self {
        Self {
            ty_env: KTyEnv::default(),
            locals: Default::default(),
            label_sigs: Default::default(),
            return_ty_opt: None,
            k_mod,
            mod_outline,
            mod_outlines,
            logger,
        }
    }
}

struct UnificationContext<'a> {
    variance: Variance,
    loc: Loc,
    ty_env: &'a KTyEnv,
    mod_outlines: &'a KModOutlines,
    logger: &'a Logger,
}

impl<'a> UnificationContext<'a> {
    fn new(
        loc: Loc,
        ty_env: &'a KTyEnv,
        mod_outlines: &'a KModOutlines,
        logger: &'a Logger,
    ) -> Self {
        Self {
            variance: Variance::Co,
            loc,
            ty_env,
            mod_outlines,
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

    fn bug_unresolved(&self, other: &KTy2, cause: KTyCause) {
        log::error!(
            "unresolved な型は、単一化中に束縛できるように、型検査の前にメタ型変数に置き換えておく必要があります (other={:?}, loc={:?}, cause={:?})",
            other.display(self.ty_env, self.mod_outlines),
            self.loc,cause
        );
    }

    fn error_ptr_mut(&self) {
        self.logger
            .error(self.loc, "ポインタの可変性に互換性がありません");
    }

    fn error_arity(&self, left: &KTy2, right: &KTy2) {
        // FIXME: エラーメッセージを改善
        self.logger.error(
            self.loc,
            format!(
                "型引数の個数が一致しません {:?}",
                (
                    left.display(self.ty_env, self.mod_outlines),
                    right.display(self.ty_env, self.mod_outlines),
                )
            ),
        );
    }

    fn error_ununifiable(&self, left: &KTy2, right: &KTy2) {
        self.logger.error(
            self.loc,
            format!(
                "型が一致しません ({} <- {})",
                left.display(self.ty_env, self.mod_outlines),
                right.display(self.ty_env, self.mod_outlines),
            ),
        );
    }

    fn error_unresolved_alias(&self, k_mod: KMod, alias: KAlias) {
        self.logger.error(
            self.loc,
            format!(
                "解決されないエイリアスです ({})",
                KTy2::Alias(k_mod, alias).display(self.ty_env, self.mod_outlines),
            ),
        );
    }
}

fn can_cast_number(
    _left: &KNumberTy,
    _right: &KNumberTy,
    _ux: &mut UnificationContext<'_>,
) -> bool {
    // FIXME: right が不特定型(INN/UNN/FNN/CNN)なら左辺の型へのキャストを試みる。丸め誤差が生じたりオーバーフローが起こるなら false。変性に注意
    true
}

fn do_unify2(left: &KTy2, right: &KTy2, ux: &mut UnificationContext<'_>) {
    match (left, right) {
        (KTy2::Unresolved { cause }, other) | (other, KTy2::Unresolved { cause }) => {
            ux.bug_unresolved(other, *cause);
        }

        // never は任意の型に upcast できる。
        (_, KTy2::Never) if ux.variance == Variance::Co => {}
        (KTy2::Never, _) if ux.variance == Variance::Contra => {}

        // 左右の型が完全に一致するケース:
        (KTy2::Meta(..), KTy2::Meta(..))
        | (KTy2::Unit, KTy2::Unit)
        | (KTy2::Number(..), KTy2::Number(..))
        | (KTy2::Alias(..), KTy2::Alias(..))
        | (KTy2::ConstEnum(..), KTy2::ConstEnum(..))
        | (KTy2::StructEnum(..), KTy2::StructEnum(..))
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

        // エイリアスの展開
        (KTy2::Alias(k_mod, alias), _) => {
            match alias
                .of(&k_mod.of(ux.mod_outlines).aliases)
                .referent_as_ty()
            {
                Some(ty) => do_unify2(&ty, right, ux),
                None => ux.error_unresolved_alias(*k_mod, *alias),
            }
        }
        (_, KTy2::Alias(k_mod, alias)) => {
            match alias
                .of(&k_mod.of(ux.mod_outlines).aliases)
                .referent_as_ty()
            {
                Some(ty) => do_unify2(left, &ty, ux),
                None => ux.error_unresolved_alias(*k_mod, *alias),
            }
        }

        // その他、型ごとのルール
        (KTy2::Number(left), KTy2::Number(right)) if can_cast_number(left, right, ux) => {}
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
        | (KTy2::Unit, _)
        | (KTy2::Number(_), _)
        | (KTy2::Ptr { .. }, _)
        | (KTy2::Fn { .. }, _)
        | (KTy2::ConstEnum(..), _)
        | (KTy2::StructEnum(..), _)
        | (KTy2::Struct(..), _) => {
            ux.error_ununifiable(left, right);
        }
    }
}

/// left 型の変数に right 型の値を代入できるか判定する。
/// 必要に応じて型変数を束縛する。
fn unify2(left: &KTy2, right: &KTy2, loc: Loc, tx: &mut Tx) {
    let mut ux = UnificationContext::new(loc, &tx.ty_env, tx.mod_outlines, &tx.logger);
    do_unify2(&left, &right, &mut ux);
}

fn fresh_meta_ty(loc: Loc, tx: &mut Tx) -> KTy2 {
    let meta_ty = tx.ty_env.alloc(KMetaTyData::new(RefCell::default(), loc));
    KTy2::Meta(meta_ty)
}

fn resolve_symbol_def2(symbol: &mut KSymbol, expected_ty_opt: Option<&KTy2>, tx: &mut Tx) {
    if symbol.ty(&tx.locals).is_unresolved() {
        let expected_ty = match expected_ty_opt {
            None => fresh_meta_ty(symbol.loc(), tx),
            Some(ty) => ty.clone(),
        };

        *symbol.ty_mut(&mut tx.locals) = expected_ty;
        return;
    }

    if let Some(expected_ty) = expected_ty_opt {
        let symbol_ty = symbol.ty(&tx.locals);
        unify2(&symbol_ty, expected_ty, symbol.loc(), tx);
    }
}

fn resolve_symbol_use(symbol: &mut KSymbol, tx: &mut Tx) -> KTy2 {
    let current_ty = symbol.ty(&tx.locals);
    if current_ty.is_unresolved() {
        error!("def_ty is unresolved. symbol is undefined? {:?}", symbol);
    }

    current_ty
}

fn resolve_pat(pat: &mut KTerm, expected_ty: &KTy2, tx: &mut Tx) {
    match pat {
        KTerm::Name(symbol) => {
            resolve_symbol_def2(symbol, Some(expected_ty), tx);
        }
        _ => {
            resolve_term(pat, tx);
        }
    }
}

fn resolve_alias_term(alias: KAlias, loc: Loc, tx: &mut Tx) -> KTy2 {
    let outline = match alias
        .of(&tx.mod_outline.aliases)
        .referent_outline(&tx.mod_outlines)
    {
        Some(outline) => outline,
        None => {
            tx.logger.error(
                loc,
                "解決されていないエイリアスの型が {unresolved} になりました",
            );
            return KTy2::Unresolved {
                cause: KTyCause::Alias,
            };
        }
    };

    match outline {
        KProjectSymbolOutline::Mod(..) => {
            tx.logger.error(
                loc,
                "モジュールを指すエイリアスを値として使うことはできません",
            );
            KTy2::Never
        }
        KProjectSymbolOutline::Struct(_) => todo!(),
        KProjectSymbolOutline::ModLocal {
            k_mod,
            symbol_outline,
            ..
        } => match symbol_outline {
            KModLocalSymbolOutline::Alias(alias, alias_data) => {
                resolve_alias_term(alias, alias_data.loc(), tx)
            }
            KModLocalSymbolOutline::Const(_, const_data) => const_data.value_ty.to_ty2(k_mod),
            KModLocalSymbolOutline::StaticVar(_, static_var_outline) => {
                static_var_outline.ty.to_ty2(k_mod)
            }
            KModLocalSymbolOutline::Fn(_, fn_outline) => fn_outline.ty().to_ty2(k_mod),
            KModLocalSymbolOutline::ExternFn(_, extern_fn_outline) => {
                extern_fn_outline.ty().to_ty2(k_mod)
            }
            KModLocalSymbolOutline::ConstEnum(..)
            | KModLocalSymbolOutline::StructEnum(_, _)
            | KModLocalSymbolOutline::Struct(_, _) => {
                tx.logger
                    .unimpl(loc, "インポートされた型の型検査は未実装です");
                KTy2::Never
            }
            KModLocalSymbolOutline::LocalVar(..) => {
                tx.logger
                    .unexpected(loc, "エイリアスがローカル変数を指すことはありません");
                KTy2::Never
            }
            KModLocalSymbolOutline::Field(..) => {
                tx.logger
                    .unexpected(loc, "エイリアスがフィールドを指すことはありません");
                KTy2::Never
            }
        },
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
        KTerm::Alias { alias, loc } => resolve_alias_term(*alias, *loc, tx),
        KTerm::Const { k_mod, k_const, .. } => k_const
            .ty(&k_mod.of(&tx.mod_outlines).consts)
            .to_ty2(*k_mod),
        KTerm::StaticVar { static_var, .. } => {
            static_var.ty(&tx.mod_outline.static_vars).to_ty2(tx.k_mod)
        }
        KTerm::Fn { k_fn, .. } => k_fn.ty(&tx.mod_outline.fns).to_ty2(tx.k_mod),
        KTerm::Label { label, .. } => label.ty(&tx.label_sigs),
        KTerm::Return { .. } => tx.return_ty_opt.clone().unwrap().to_ty2(tx.k_mod),
        KTerm::ExternFn { extern_fn, .. } => {
            extern_fn.ty(&tx.mod_outline.extern_fns).to_ty2(tx.k_mod)
        }
        KTerm::Name(symbol) => resolve_symbol_use(symbol, tx),
        KTerm::RecordTag {
            k_mod, k_struct, ..
        } => {
            let mod_outline = k_mod.of(&tx.mod_outlines);
            k_struct
                .tag_ty(&mod_outline.structs, &mod_outline.struct_enums)
                .to_ty2(tx.k_mod)
        }
        KTerm::FieldTag(_) => unreachable!(),
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
            [label, args @ ..] => {
                let def_fn_ty = resolve_term(label, tx);
                let arg_tys = resolve_terms(args, tx);

                // FIXME: unwrap しない
                let (param_tys, _) = def_fn_ty.as_fn(&tx.ty_env).unwrap();

                // FIXME: 引数の個数を検査する

                for (param_ty, arg_ty) in param_tys.iter().zip(&arg_tys) {
                    unify2(param_ty, arg_ty, node.loc, tx);
                }
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([callee, args @ ..], [result]) => loop {
                let def_fn_ty = resolve_term(callee, tx);
                let arg_tys = resolve_terms(args, tx);

                // FIXME: unwrap しない
                let (param_tys, result_ty) = match def_fn_ty.as_fn(&tx.ty_env) {
                    Some(it) => it,
                    None => {
                        tx.logger
                            .error(node.loc(), "関数ではないものは呼び出せません");
                        break;
                    }
                };

                // FIXME: 引数の個数を検査する

                for (param_ty, arg_ty) in param_tys.iter().zip(&arg_tys) {
                    unify2(param_ty, arg_ty, node.loc, tx);
                }
                resolve_symbol_def2(result, Some(&result_ty), tx);
                break;
            },
            _ => unimplemented!(),
        },
        KPrim::Record => match (node.tys.as_mut_slice(), node.results.as_mut_slice()) {
            ([ty], [result]) => {
                let (k_mod, k_struct) = ty.as_struct(&tx.ty_env).unwrap();
                let mod_outline = k_mod.of(&tx.mod_outlines);

                for (arg, field) in node
                    .args
                    .iter_mut()
                    .zip(k_struct.fields(&mod_outline.structs))
                {
                    let arg_ty = resolve_term(arg, tx);
                    unify2(
                        &field.ty(&mod_outline.fields).to_ty2(k_mod),
                        &arg_ty,
                        node.loc,
                        tx,
                    );
                }

                let ty = k_struct.ty(&mod_outline.structs).to_ty2(k_mod);
                resolve_symbol_def2(result, Some(&ty), tx);

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
                    loc,
                })],
                [result],
            ) => {
                let left_ty = resolve_term(left, tx);

                let result_ty = (|| {
                    let (_, ty) = left_ty.as_ptr(&tx.ty_env)?;
                    let ty = match ty.as_struct_or_enum(&tx.ty_env)? {
                        KEnumOrStruct::Enum(k_mod, struct_enum) => struct_enum
                            .variants(&tx.mod_outline.struct_enums)
                            .iter()
                            .find_map(|&k_struct| {
                                if k_struct.name(&tx.mod_outline.structs) == field_name {
                                    Some(KTy2::Struct(k_mod, k_struct))
                                } else {
                                    None
                                }
                            })?,
                        KEnumOrStruct::Struct(k_mod, k_struct) => k_struct
                            .fields(&tx.mod_outline.structs)
                            .iter()
                            .find(|field| field.name(&tx.mod_outline.fields) == *field_name)
                            .map(|field| field.ty(&tx.mod_outline.fields).to_ty2(k_mod))?,
                    };
                    Some(ty.into_ptr(KMut::Const))
                })()
                .unwrap_or_else(|| {
                    tx.logger.error(loc, "bad type");
                    KTy2::Never
                });

                resolve_symbol_def2(result, Some(&result_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::GetFieldMut => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            (
                [left, KTerm::FieldTag(KFieldTag {
                    name: field_name,
                    loc,
                })],
                [result],
            ) => {
                let left_ty = resolve_term(left, tx);

                let result_ty = (|| {
                    let (k_mut, ty) = left_ty.as_ptr(&tx.ty_env)?;
                    if let KMut::Const = k_mut {
                        tx.logger.error(&left.loc(), "unexpected const reference");
                    }

                    let ty = match ty.as_struct_or_enum(&tx.ty_env)? {
                        KEnumOrStruct::Enum(k_mod, struct_enum) => struct_enum
                            .variants(&tx.mod_outline.struct_enums)
                            .iter()
                            .find_map(|&k_struct| {
                                if k_struct.name(&tx.mod_outline.structs) == field_name {
                                    Some(KTy2::Struct(k_mod, k_struct))
                                } else {
                                    None
                                }
                            })?,
                        KEnumOrStruct::Struct(k_mod, k_struct) => k_struct
                            .fields(&tx.mod_outline.structs)
                            .iter()
                            .find(|field| field.name(&tx.mod_outline.fields) == *field_name)
                            .map(|field| field.ty(&tx.mod_outline.fields).to_ty2(k_mod))?,
                    };
                    Some(ty.into_ptr(k_mut))
                })()
                .unwrap_or_else(|| {
                    tx.logger.error(loc, "bad type");
                    KTy2::Never
                });

                resolve_symbol_def2(result, Some(&result_ty), tx);
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
                resolve_symbol_def2(result, Some(&init_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);

                let result_ty_opt = arg_ty.as_ptr(&tx.ty_env).map(|(_, ty)| ty);
                if result_ty_opt.is_none() {
                    tx.logger.error(&result.loc(), "expected a reference");
                }
                resolve_symbol_def2(result, result_ty_opt.as_ref(), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Ref => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_symbol_def2(result, Some(&arg_ty.into_ptr(KMut::Const)), tx);
            }
            _ => unimplemented!(),
        },
        // FIXME: ref の実装と重複
        KPrim::RefMut => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                resolve_symbol_def2(result, Some(&arg_ty.into_ptr(KMut::Mut)), tx);
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
                resolve_symbol_def2(result, Some(&target_ty), tx);

                // FIXME: 同じ型へのキャストは警告?
                if let KTy2::Unresolved { .. } | KTy2::Never = arg_ty {
                    // Skip.
                } else if !arg_ty.is_primitive(&tx.ty_env) {
                    tx.logger.error(
                        &node,
                        format!("can't cast from non-primitive type {:?}", arg_ty),
                    );
                } else if !target_ty.is_primitive(&tx.ty_env) {
                    let msg = format!("can't cast to non-primitive type {:?}", target_ty);
                    tx.logger.error(&node, msg);
                }
            }
            _ => unimplemented!(),
        },
        KPrim::Minus => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                // FIXME: bool or iNN
                resolve_symbol_def2(result, Some(&arg_ty), tx);
            }
            _ => unimplemented!(),
        },
        KPrim::Not => match (node.args.as_mut_slice(), node.results.as_mut_slice()) {
            ([arg], [result]) => {
                let arg_ty = resolve_term(arg, tx);
                // FIXME: bool or iNN or uNN
                resolve_symbol_def2(result, Some(&arg_ty), tx);
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

                resolve_symbol_def2(result, Some(&left_ty), tx);
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

                resolve_symbol_def2(result, Some(&left_ty), tx);
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

                    resolve_symbol_def2(result, Some(&KTy2::BOOL), tx);
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
                        tx.logger.error(&left.loc(), "expected mutable reference");
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

                    // 左辺は lval なのでポインタ型のはず
                    let (k_mut, left_ty) = left_ty.as_ptr(&tx.ty_env).unwrap();
                    if let KMut::Const = k_mut {
                        tx.logger.error(&left.loc(), "unexpected const reference");
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
                    let (k_mut, left_ty) = left_ty.as_ptr(&tx.ty_env).unwrap();
                    if let KMut::Const = k_mut {
                        tx.logger.error(&left.loc(), "unexpected const reference");
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
    assert!(fn_data.ty_env.is_empty());

    for i in 0..fn_data.params.len() {
        let param = &mut fn_data.params[i];
        let param_ty = &k_fn.param_tys(&tx.mod_outline.fns)[i].to_ty2(tx.k_mod);
        resolve_symbol_def2(param, Some(param_ty), tx);
    }

    // いまから生成するところなので空のはず。
    assert!(fn_data.label_sigs.is_empty());

    for label_data in fn_data.labels.iter_mut() {
        for param in &mut label_data.params {
            resolve_symbol_def2(param, None, tx);
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
    for i in 0..data.params.len() {
        let param = &mut data.params[i];
        let param_ty = &extern_fn.param_tys(&tx.mod_outline.extern_fns)[i].to_ty2(tx.k_mod);
        resolve_symbol_def2(param, Some(&param_ty), tx);
    }
}

fn prepare_struct(k_struct: KStruct, tx: &mut Tx) {
    for field in k_struct.fields(&tx.mod_outline.structs) {
        if field.ty(&tx.mod_outline.fields).is_unresolved() {
            // FIXME: handle correctly. unresolved type crashes on unification for now
            tx.logger
                .error(&field.loc(&tx.mod_outline.fields), "unresolved field type");
        }
    }
}

fn resolve_root(root: &mut KModData, tx: &mut Tx) {
    for k_struct in tx.mod_outline.structs.keys() {
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
        tx.return_ty_opt = Some(k_fn.return_ty(&tx.mod_outline.fns));
        swap(&mut tx.locals, &mut fn_data.locals);
        swap(&mut tx.label_sigs, &mut fn_data.label_sigs);
        swap(&mut tx.ty_env, &mut fn_data.ty_env);

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
    mod_outline: &KModOutline,
    mod_data: &mut KModData,
    mod_outlines: &KModOutlines,
    logger: Logger,
) {
    let mut tx = Tx::new(k_mod, mod_outline, mod_outlines, logger);
    resolve_root(mod_data, &mut tx);
}
