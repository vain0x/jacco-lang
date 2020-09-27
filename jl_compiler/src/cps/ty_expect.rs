#![allow(unused)]

use super::*;

/// 式の型に与えられる制約。
#[derive(Copy, Clone)]
pub(crate) enum TyExpect<'a> {
    /// 未実装部分
    Todo,
    NumberOrPtr,
    IsizeOrUsize,
    Exact(&'a KTy2),
}

impl<'a> TyExpect<'a> {
    pub(crate) fn unit() -> TyExpect<'static> {
        TyExpect::Exact(&KTy2::Unit)
    }

    pub(crate) fn from(ty: &'a KTy2) -> Self {
        if ty.is_unresolved() {
            Self::Todo
        } else {
            TyExpect::Exact(ty)
        }
    }

    pub(crate) fn as_number(self) -> Option<KNumberTy> {
        match self {
            TyExpect::Exact(KTy2::Number(it)) => Some(*it),
            _ => None,
        }
    }

    pub(crate) fn meet(self, other: Self) -> Self {
        // TODO: ちゃんと実装する
        match self {
            TyExpect::Todo => other,
            _ => self,
        }
    }

    pub(crate) fn display(self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> String {
        match self {
            TyExpect::Todo => "TODO".into(),
            TyExpect::NumberOrPtr => "(iNN | uNN | fNN | cNN | *unknown | *mut unknown)".into(),
            TyExpect::IsizeOrUsize => "(isize | usize)".into(),
            TyExpect::Exact(ty) => ty.display(ty_env, mod_outline),
        }
    }
}
