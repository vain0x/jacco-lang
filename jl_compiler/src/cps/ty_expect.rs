#![allow(unused)]

use super::*;

/// 式の型に与えられる制約。
#[derive(Clone)]
pub(crate) enum TyExpect {
    /// 未実装部分
    Todo,
    Unknown,
    BoolOrInt,
    Number,
    NumberOrPtr,
    IsizeOrUsize,
    Exact(KTy2),
}

impl TyExpect {
    pub(crate) fn unit() -> TyExpect {
        TyExpect::Exact(KTy2::Unit)
    }

    pub(crate) fn bool() -> TyExpect {
        TyExpect::Exact(KTy2::BOOL)
    }

    pub(crate) fn from(ty: &KTy2) -> Self {
        if ty.is_unresolved() {
            Self::Todo
        } else {
            TyExpect::Exact(ty.clone())
        }
    }

    pub(crate) fn as_number(&self) -> Option<KNumberTy> {
        match self {
            TyExpect::Exact(KTy2::Number(it)) => Some(*it),
            _ => None,
        }
    }

    pub(crate) fn try_unwrap_ptr(self, ty_env: &KTyEnv) -> Self {
        match self {
            TyExpect::NumberOrPtr => TyExpect::Unknown,
            TyExpect::Exact(ty) => match ty.as_ptr(ty_env) {
                Some((_, ty)) => {
                    // FIXME: TyExpect に型を所有させる必要がある？
                    // TyExpect::Exact(&ty),
                    TyExpect::Todo
                }
                None => TyExpect::Unknown,
            },
            _ => TyExpect::Todo,
        }
    }

    pub(crate) fn meet(self, other: Self) -> Self {
        // TODO: ちゃんと実装する
        match self {
            TyExpect::Todo => other,
            _ => self,
        }
    }

    pub(crate) fn display(&self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> String {
        match self {
            TyExpect::Todo => "TODO".into(),
            TyExpect::Unknown => "unknown".into(),
            TyExpect::BoolOrInt => "(bool | iNN | uNN | cNN)".into(),
            TyExpect::Number => "(iNN | uNN | fNN | cNN)".into(),
            TyExpect::NumberOrPtr => "(iNN | uNN | fNN | cNN | *unknown | *mut unknown)".into(),
            TyExpect::IsizeOrUsize => "(isize | usize)".into(),
            TyExpect::Exact(ty) => ty.display(ty_env, mod_outline),
        }
    }
}
