use std::fmt::{self, Debug, Formatter};

/// 組み込みの数値型
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum KNumberTy {
    Bool,
    I8,
    I16,
    I32,
    I64,
    Isize,
    #[allow(unused)]
    INN,
    U8,
    U16,
    U32,
    U64,
    Usize,
    #[allow(unused)]
    UNN,
    F32,
    F64,
    #[allow(unused)]
    FNN,
    /// UTF-8 のコードユニット。(符号なし8ビット整数。C++ の `char8_t`)
    C8,
    /// UTF-16 のコードユニット
    C16,
    /// Unicode scalar value. (Rust の `char`)
    C32,
    #[allow(unused)]
    CNN,
}

impl KNumberTy {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            KNumberTy::Bool => "bool",
            KNumberTy::I8 => "i8",
            KNumberTy::I16 => "i16",
            KNumberTy::I32 => "i32",
            KNumberTy::I64 => "i64",
            KNumberTy::Isize => "isize",
            KNumberTy::INN => "{iNN}",
            KNumberTy::U8 => "u8",
            KNumberTy::U16 => "u16",
            KNumberTy::U32 => "u32",
            KNumberTy::U64 => "u64",
            KNumberTy::Usize => "usize",
            KNumberTy::UNN => "{uNN}",
            KNumberTy::F32 => "f32",
            KNumberTy::F64 => "f64",
            KNumberTy::FNN => "{fNN}",
            KNumberTy::C8 => "c8",
            KNumberTy::C16 => "c16",
            KNumberTy::C32 => "c32",
            KNumberTy::CNN => "{cNN}",
        }
    }

    #[allow(unused)]
    pub(crate) fn parse(s: &str) -> Option<KNumberTy> {
        let ty = match s {
            "bool" => KNumberTy::Bool,
            "i8" => KNumberTy::I8,
            "i16" => KNumberTy::I16,
            "i32" => KNumberTy::I32,
            "i64" => KNumberTy::I64,
            "isize" => KNumberTy::Isize,
            "u8" => KNumberTy::U8,
            "u16" => KNumberTy::U16,
            "u32" => KNumberTy::U32,
            "u64" => KNumberTy::U64,
            "usize" => KNumberTy::Usize,
            "f32" => KNumberTy::F32,
            "f64" => KNumberTy::F64,
            "c8" => KNumberTy::C8,
            "c16" => KNumberTy::C16,
            "c32" => KNumberTy::C32,
            _ => return None,
        };
        Some(ty)
    }
}

impl Debug for KNumberTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
