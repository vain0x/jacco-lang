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
