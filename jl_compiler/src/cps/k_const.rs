use super::{KConstEnum, KEnum, KNode, KNumberTy, KTerm, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId, VecArenaSlice},
};
use std::fmt::{self, Debug, Formatter};

pub(crate) struct KConstTag;

pub(crate) type KConst = VecArenaId<KConstTag>;

pub(crate) type KConsts = VecArenaSlice<KConstTag>;

pub(crate) type KConstArena = VecArena<KConstTag, KConstData>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum KConstParent {
    #[allow(unused)]
    Enum(KEnum),
    ConstEnum(KConstEnum),
}

impl KConst {
    /// 値の型、またはこの定数が属する enum の型
    pub(crate) fn ty(self, consts: &KConstArena) -> KTy {
        match consts[self].parent_opt {
            Some(KConstParent::Enum(k_enum)) => KTy::Enum(k_enum),
            Some(KConstParent::ConstEnum(const_enum)) => KTy::ConstEnum(const_enum),
            None => consts[self].value_ty.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KConstData {
    pub(crate) name: String,
    pub(crate) value_ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
    pub(crate) parent_opt: Option<KConstParent>,
    pub(crate) loc: Loc,
}

pub(crate) type KConstInits = VecArena<KConstTag, KConstInit>;

#[derive(Clone, Debug)]
pub(crate) struct KConstInit {
    pub(crate) init_opt: Option<(KNode, KTerm)>,
}

impl KConstInit {
    pub(crate) fn new_empty() -> Self {
        Self { init_opt: None }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum KConstValue {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Isize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Usize(usize),
    F32(f32),
    F64(f64),
    C8(u8),
    C16(u16),
    C32(u32),
}

impl KConstValue {
    pub(crate) fn cast_as_usize(&self) -> usize {
        match self {
            KConstValue::Bool(value) => *value as usize,
            KConstValue::I32(value) => *value as usize,
            KConstValue::I64(value) => *value as usize,
            KConstValue::U64(value) => *value as usize,
            KConstValue::Usize(value) => *value,
            KConstValue::F64(value) => *value as usize,
            _ => unimplemented!(),
        }
    }

    #[allow(unused)]
    pub(crate) fn ty(&self) -> KTy {
        match self {
            KConstValue::Bool(_) => KTy::BOOL,
            KConstValue::I32(_) => KTy::I32,
            KConstValue::I64(_) => KTy::I64,
            KConstValue::Usize(_) => KTy::USIZE,
            KConstValue::F64(_) => KTy::F64,
            _ => unimplemented!(),
        }
    }
}

impl From<(KNumber, KNumberTy)> for KConstValue {
    fn from((number, ty): (KNumber, KNumberTy)) -> Self {
        match (number, ty) {
            (KNumber::INN(value), KNumberTy::Bool) => KConstValue::Bool(value != 0),
            (KNumber::UNN(value), KNumberTy::Bool) => KConstValue::Bool(value != 0),
            (KNumber::CNN(value), KNumberTy::Bool) => KConstValue::Bool(value != 0),
            (KNumber::FNN(_), KNumberTy::Bool) => KConstValue::Bool(true),
            (KNumber::INN(value), KNumberTy::I8) => KConstValue::I8(value as i8),
            (KNumber::INN(value), KNumberTy::I16) => KConstValue::I16(value as i16),
            (KNumber::INN(value), KNumberTy::I32) => KConstValue::I32(value as i32),
            (KNumber::INN(value), KNumberTy::I64) => KConstValue::I64(value as i64),
            (KNumber::INN(value), KNumberTy::Isize) => KConstValue::Isize(value as isize),
            (KNumber::INN(value), KNumberTy::INN) => KConstValue::I64(value),
            (KNumber::INN(value), KNumberTy::U8) => KConstValue::U8(value as u8),
            (KNumber::INN(value), KNumberTy::U16) => KConstValue::U16(value as u16),
            (KNumber::INN(value), KNumberTy::U32) => KConstValue::U32(value as u32),
            (KNumber::INN(value), KNumberTy::U64) => KConstValue::U64(value as u64),
            (KNumber::INN(value), KNumberTy::Usize) => KConstValue::Usize(value as usize),
            (KNumber::INN(value), KNumberTy::UNN) => KConstValue::U64(value as u64),
            (KNumber::INN(value), KNumberTy::F32) => KConstValue::F32(value as f32),
            (KNumber::INN(value), KNumberTy::F64) => KConstValue::F64(value as f64),
            (KNumber::INN(value), KNumberTy::FNN) => KConstValue::F64(value as f64),
            (KNumber::INN(value), KNumberTy::C8) => KConstValue::C8(value as u8),
            (KNumber::INN(value), KNumberTy::C16) => KConstValue::C16(value as u16),
            (KNumber::INN(value), KNumberTy::C32) => KConstValue::C32(value as u32),
            (KNumber::INN(value), KNumberTy::CNN) => KConstValue::C32(value as u32),
            (KNumber::UNN(value), KNumberTy::I8) => KConstValue::I8(value as i8),
            (KNumber::UNN(value), KNumberTy::I16) => KConstValue::I16(value as i16),
            (KNumber::UNN(value), KNumberTy::I32) => KConstValue::I32(value as i32),
            (KNumber::UNN(value), KNumberTy::I64) => KConstValue::I64(value as i64),
            (KNumber::UNN(value), KNumberTy::Isize) => KConstValue::Isize(value as isize),
            (KNumber::UNN(value), KNumberTy::INN) => KConstValue::I64(value as i64),
            (KNumber::UNN(value), KNumberTy::U8) => KConstValue::U8(value as u8),
            (KNumber::UNN(value), KNumberTy::U16) => KConstValue::U16(value as u16),
            (KNumber::UNN(value), KNumberTy::U32) => KConstValue::U32(value as u32),
            (KNumber::UNN(value), KNumberTy::U64) => KConstValue::U64(value as u64),
            (KNumber::UNN(value), KNumberTy::Usize) => KConstValue::Usize(value as usize),
            (KNumber::UNN(value), KNumberTy::UNN) => KConstValue::U64(value as u64),
            (KNumber::UNN(value), KNumberTy::F32) => KConstValue::F32(value as f32),
            (KNumber::UNN(value), KNumberTy::F64) => KConstValue::F64(value as f64),
            (KNumber::UNN(value), KNumberTy::FNN) => KConstValue::F64(value as f64),
            (KNumber::UNN(value), KNumberTy::C8) => KConstValue::C8(value as u8),
            (KNumber::UNN(value), KNumberTy::C16) => KConstValue::C16(value as u16),
            (KNumber::UNN(value), KNumberTy::C32) => KConstValue::C32(value as u32),
            (KNumber::UNN(value), KNumberTy::CNN) => KConstValue::C32(value as u32),
            (KNumber::FNN(value), KNumberTy::I8) => KConstValue::I8(value as i8),
            (KNumber::FNN(value), KNumberTy::I16) => KConstValue::I16(value as i16),
            (KNumber::FNN(value), KNumberTy::I32) => KConstValue::I32(value as i32),
            (KNumber::FNN(value), KNumberTy::I64) => KConstValue::I64(value as i64),
            (KNumber::FNN(value), KNumberTy::Isize) => KConstValue::Isize(value as isize),
            (KNumber::FNN(value), KNumberTy::INN) => KConstValue::I64(value as i64),
            (KNumber::FNN(value), KNumberTy::U8) => KConstValue::U8(value as u8),
            (KNumber::FNN(value), KNumberTy::U16) => KConstValue::U16(value as u16),
            (KNumber::FNN(value), KNumberTy::U32) => KConstValue::U32(value as u32),
            (KNumber::FNN(value), KNumberTy::U64) => KConstValue::U64(value as u64),
            (KNumber::FNN(value), KNumberTy::Usize) => KConstValue::Usize(value as usize),
            (KNumber::FNN(value), KNumberTy::UNN) => KConstValue::U64(value as u64),
            (KNumber::FNN(value), KNumberTy::F32) => KConstValue::F32(value as f32),
            (KNumber::FNN(value), KNumberTy::F64) => KConstValue::F64(value as f64),
            (KNumber::FNN(value), KNumberTy::FNN) => KConstValue::F64(value as f64),
            (KNumber::FNN(value), KNumberTy::C8) => KConstValue::C8(value as u8),
            (KNumber::FNN(value), KNumberTy::C16) => KConstValue::C16(value as u16),
            (KNumber::FNN(value), KNumberTy::C32) => KConstValue::C32(value as u32),
            (KNumber::FNN(value), KNumberTy::CNN) => KConstValue::C32(value as u32),
            (KNumber::CNN(value), KNumberTy::I8) => KConstValue::I8(value as i8),
            (KNumber::CNN(value), KNumberTy::I16) => KConstValue::I16(value as i16),
            (KNumber::CNN(value), KNumberTy::I32) => KConstValue::I32(value as i32),
            (KNumber::CNN(value), KNumberTy::I64) => KConstValue::I64(value as i64),
            (KNumber::CNN(value), KNumberTy::Isize) => KConstValue::Isize(value as isize),
            (KNumber::CNN(value), KNumberTy::INN) => KConstValue::I64(value as i64),
            (KNumber::CNN(value), KNumberTy::U8) => KConstValue::U8(value as u8),
            (KNumber::CNN(value), KNumberTy::U16) => KConstValue::U16(value as u16),
            (KNumber::CNN(value), KNumberTy::U32) => KConstValue::U32(value as u32),
            (KNumber::CNN(value), KNumberTy::U64) => KConstValue::U64(value as u64),
            (KNumber::CNN(value), KNumberTy::Usize) => KConstValue::Usize(value as usize),
            (KNumber::CNN(value), KNumberTy::UNN) => KConstValue::U64(value as u64),
            (KNumber::CNN(value), KNumberTy::F32) => KConstValue::F32(value as f32),
            (KNumber::CNN(value), KNumberTy::F64) => KConstValue::F64(value as f64),
            (KNumber::CNN(value), KNumberTy::FNN) => KConstValue::F64(value as f64),
            (KNumber::CNN(value), KNumberTy::C8) => KConstValue::C8(value as u8),
            (KNumber::CNN(value), KNumberTy::C16) => KConstValue::C16(value as u16),
            (KNumber::CNN(value), KNumberTy::C32) => KConstValue::C32(value as u32),
            (KNumber::CNN(value), KNumberTy::CNN) => KConstValue::C32(value as u32),
        }
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
