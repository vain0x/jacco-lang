use crate::{cps::*, source::Loc};
use std::mem::replace;

pub(crate) struct UnificationContext<'a> {
    variance: Variance,
    loc: Loc,
    ty_env: &'a KTyEnv,
    mod_outline: &'a KModOutline,
    logger: &'a Logger,
}

impl<'a> UnificationContext<'a> {
    pub(crate) fn new(
        loc: Loc,
        ty_env: &'a KTyEnv,
        mod_outline: &'a KModOutline,
        logger: &'a Logger,
    ) -> Self {
        Self {
            variance: Variance::Co,
            loc,
            ty_env,
            mod_outline,
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
            other.display(self.ty_env, self.mod_outline),
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
                    left.display(self.ty_env, self.mod_outline),
                    right.display(self.ty_env, self.mod_outline),
                )
            ),
        );
    }

    fn error_ununifiable(&self, left: &KTy2, right: &KTy2) {
        self.logger.error(
            self.loc,
            format!(
                "型が一致しません ({} <- {})",
                left.display(self.ty_env, self.mod_outline),
                right.display(self.ty_env, self.mod_outline),
            ),
        );
    }

    pub(crate) fn unify(&mut self, left: &KTy2, right: &KTy2) {
        do_unify2(left, right, self)
    }
}

fn do_unify2(left: &KTy2, right: &KTy2, ux: &mut UnificationContext<'_>) {
    match (left, right) {
        (KTy2::Unresolved { cause }, other) | (other, KTy2::Unresolved { cause }) => {
            ux.bug_unresolved(other, *cause);
        }

        // 任意の型は unknown に upcast できる。
        (KTy2::Unknown, _) if ux.variance == Variance::Co => {}
        (_, KTy2::Unknown) if ux.variance == Variance::Contra => {}

        // never は任意の型に upcast できる。
        (_, KTy2::Never) if ux.variance == Variance::Co => {}
        (KTy2::Never, _) if ux.variance == Variance::Contra => {}

        // 左右の型が完全に一致するケース:
        (KTy2::Meta(..), KTy2::Meta(..))
        | (KTy2::Var(..), KTy2::Var(..))
        | (KTy2::Unknown, KTy2::Unknown)
        | (KTy2::Unit, KTy2::Unit)
        | (KTy2::Number(..), KTy2::Number(..))
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
        (
            KTy2::App { k_struct, ty_args },
            KTy2::App {
                k_struct: right_struct,
                ty_args: right_args,
            },
        ) if k_struct == right_struct => {
            // 型引数に互換性があるか？
            let mut ty_args = ty_args.iter().collect::<Vec<_>>();
            ty_args.sort_by_key(|&(key, _)| key);
            let mut right_args = right_args.iter().collect::<Vec<_>>();
            right_args.sort_by_key(|&(key, _)| key);

            ux.do_with_invariant(|ux| {
                for ((_, left), (_, right)) in ty_args.iter().zip(right_args) {
                    do_unify2(left, right, ux);
                }
            });
        }

        // 不一致
        (KTy2::Var(..), _)
        | (KTy2::Never, _)
        | (KTy2::Unknown, _)
        | (KTy2::Unit, _)
        | (KTy2::Number(_), _)
        | (KTy2::Ptr { .. }, _)
        | (KTy2::Fn { .. }, _)
        | (KTy2::ConstEnum(..), _)
        | (KTy2::StructEnum(..), _)
        | (KTy2::Struct(..), _)
        | (KTy2::App { .. }, _) => {
            ux.error_ununifiable(left, right);
        }
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
