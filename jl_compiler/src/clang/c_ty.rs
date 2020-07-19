use crate::cps::KNumberTy;

#[derive(Clone)]
pub(crate) enum CTy {
    Other(&'static str),
    Void,
    SignedChar,
    UnsignedChar,
    Short,
    UnsignedShort,
    Int,
    UnsignedInt,
    LongLong,
    UnsignedLongLong,
    Float,
    Double,
    Ptr {
        ty: Box<CTy>,
    },
    Const {
        ty: Box<CTy>,
    },
    #[allow(unused)]
    Enum(String),
    Struct(String),
}

impl CTy {
    pub(crate) fn into_const(self) -> CTy {
        CTy::Const { ty: Box::new(self) }
    }

    pub(crate) fn into_ptr(self) -> CTy {
        CTy::Ptr { ty: Box::new(self) }
    }
}

impl From<KNumberTy> for CTy {
    fn from(ty: KNumberTy) -> Self {
        match ty {
            KNumberTy::I8 => CTy::SignedChar,
            KNumberTy::I16 => CTy::Short,
            KNumberTy::I32 | KNumberTy::Bool => CTy::Int,
            KNumberTy::I64 | KNumberTy::Isize | KNumberTy::INN => CTy::LongLong,
            KNumberTy::U8 | KNumberTy::C8 | KNumberTy::CNN => CTy::UnsignedChar,
            KNumberTy::U16 | KNumberTy::C16 => CTy::UnsignedShort,
            KNumberTy::U32 | KNumberTy::C32 => CTy::UnsignedInt,
            KNumberTy::U64 | KNumberTy::Usize | KNumberTy::UNN => CTy::UnsignedLongLong,
            KNumberTy::F32 => CTy::Float,
            KNumberTy::F64 | KNumberTy::FNN => CTy::Double,
        }
    }
}
