use super::{KEnum, KTy};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KConstTag;

pub(crate) type KConst = VecArenaId<KConstTag>;

pub(crate) type KConstArena = VecArena<KConstTag, KConstData>;

impl KConst {
    /// 値の型、またはこの定数が属する enum の型
    pub(crate) fn ty(self, consts: &KConstArena) -> KTy {
        match consts[self].parent_opt {
            Some(k_enum) => KTy::Enum(k_enum),
            None => consts[self].value_ty.clone(),
        }
    }

    pub(crate) fn has_value(self, consts: &KConstArena) -> bool {
        consts[self].value_opt.is_some()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KConstData {
    pub(crate) name: String,
    pub(crate) value_ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
    pub(crate) parent_opt: Option<KEnum>,
    pub(crate) location: Location,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum KConstValue {
    I32(i32),
    I64(i64),
    Usize(usize),
    F64(f64),
    Bool(bool),
}

impl KConstValue {
    pub(crate) fn cast_as_usize(&self) -> usize {
        match self {
            KConstValue::I32(value) => *value as usize,
            KConstValue::I64(value) => *value as usize,
            KConstValue::Usize(value) => *value,
            KConstValue::F64(value) => *value as usize,
            KConstValue::Bool(value) => *value as usize,
        }
    }

    #[allow(unused)]
    pub(crate) fn ty(&self) -> KTy {
        match self {
            KConstValue::I32(_) => KTy::I32,
            KConstValue::I64(_) => KTy::I64,
            KConstValue::Usize(_) => KTy::USIZE,
            KConstValue::F64(_) => KTy::F64,
            KConstValue::Bool(_) => KTy::BOOL,
        }
    }
}
