pub(crate) enum CTy {
    Other(&'static str),
    Void,
    UnsignedChar,
    Int,
    LongLong,
    Ptr { ty: Box<CTy> },
    Struct(String),
}

impl CTy {
    pub(crate) fn into_ptr(self) -> CTy {
        CTy::Ptr { ty: Box::new(self) }
    }
}
