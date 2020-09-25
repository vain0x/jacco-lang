#![allow(unused)]

use super::*;

/// 式の型に与えられる制約。
#[derive(Copy, Clone)]
pub(crate) enum TyExpect<'a> {
    /// 未実装部分
    Todo,
    Exact(&'a KTy2),
}

impl<'a> TyExpect<'a> {
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
}
