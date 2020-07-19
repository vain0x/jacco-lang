use super::{KBasicTy, KEnum, KTy, KTy2};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};
use std::fmt::{self, Debug, Formatter};

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

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum KNumberTy {
    I8,
    I16,
    I32,
    I64,
    Isize,
    INN,
    U8,
    U16,
    U32,
    U64,
    Usize,
    UNN,
    F32,
    F64,
    FNN,
    C8,
    C16,
    C32,
    CNN,
}

impl From<KNumberTy> for KBasicTy {
    fn from(ty: KNumberTy) -> Self {
        match ty {
            KNumberTy::I8 => KBasicTy::I8,
            KNumberTy::I16 => KBasicTy::I16,
            KNumberTy::I32 => KBasicTy::I32,
            KNumberTy::I64 => KBasicTy::I64,
            KNumberTy::Isize => KBasicTy::Isize,
            KNumberTy::INN => KBasicTy::INN,
            KNumberTy::U8 => KBasicTy::U8,
            KNumberTy::U16 => KBasicTy::U16,
            KNumberTy::U32 => KBasicTy::U32,
            KNumberTy::U64 => KBasicTy::U64,
            KNumberTy::Usize => KBasicTy::Usize,
            KNumberTy::UNN => KBasicTy::UNN,
            KNumberTy::F32 => KBasicTy::F32,
            KNumberTy::F64 => KBasicTy::F64,
            KNumberTy::FNN => KBasicTy::FNN,
            KNumberTy::C8 => KBasicTy::C8,
            KNumberTy::C16 => KBasicTy::C16,
            KNumberTy::C32 => KBasicTy::C32,
            KNumberTy::CNN => KBasicTy::CNN,
        }
    }
}

impl From<KNumberTy> for KTy2 {
    fn from(ty: KNumberTy) -> Self {
        KTy2::Basic(KBasicTy::from(ty))
    }
}

impl Debug for KNumberTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&KBasicTy::from(*self), f)
    }
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum KNumber {
    INN(i64),
    UNN(u64),
    FNN(f64),
    CNN(u32),
}

impl Debug for KNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KNumber::INN(value) => Debug::fmt(value, f),
            KNumber::UNN(value) => Debug::fmt(value, f),
            KNumber::FNN(value) => Debug::fmt(value, f),
            KNumber::CNN(value) => Debug::fmt(value, f),
        }
    }
}

#[allow(unused)]
#[derive(Copy, Clone, PartialEq)]
pub(crate) enum KBasicValue {
    Unit,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Isize(isize),
    INN(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Usize(usize),
    UNN(u64),
    F32(f32),
    F64(f64),
    FNN(f64),
    C8(u8),
    C16(u16),
    C32(u32),
    CNN(u32),
    Bool(bool),
}

impl Debug for KBasicValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KBasicValue::Unit => write!(f, "()"),
            KBasicValue::I8(value) => Debug::fmt(&value, f),
            KBasicValue::I16(value) => Debug::fmt(&value, f),
            KBasicValue::I32(value) => Debug::fmt(&value, f),
            KBasicValue::I64(value) => Debug::fmt(&value, f),
            KBasicValue::Isize(value) => Debug::fmt(&value, f),
            KBasicValue::INN(value) => Debug::fmt(&value, f),
            KBasicValue::U8(value) => Debug::fmt(&value, f),
            KBasicValue::U16(value) => Debug::fmt(&value, f),
            KBasicValue::U32(value) => Debug::fmt(&value, f),
            KBasicValue::U64(value) => Debug::fmt(&value, f),
            KBasicValue::Usize(value) => Debug::fmt(&value, f),
            KBasicValue::UNN(value) => Debug::fmt(&value, f),
            KBasicValue::F32(value) => Debug::fmt(&value, f),
            KBasicValue::F64(value) => Debug::fmt(&value, f),
            KBasicValue::FNN(value) => Debug::fmt(&value, f),
            KBasicValue::C8(value) => Debug::fmt(&value, f),
            KBasicValue::C16(value) => Debug::fmt(&value, f),
            KBasicValue::C32(value) => Debug::fmt(&value, f),
            KBasicValue::CNN(value) => Debug::fmt(&value, f),
            KBasicValue::Bool(value) => Debug::fmt(&value, f),
        }
    }
}
