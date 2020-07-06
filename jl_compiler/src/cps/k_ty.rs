use super::{KEnum, KMetaTy, KMut, KStruct};
use std::fmt::{self, Debug, Formatter};

#[derive(Clone)]
pub(crate) enum KTy {
    Unresolved,
    Meta(KMetaTy),
    Never,
    Unit,
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    /// UTF-8 のコードユニット。(符号なし8ビット整数。C++ の `char8_t`)
    C8,
    /// UTF-16 のコードユニット
    C16,
    /// Unicode scalar value. (Rust の `char`)
    C32,
    Bool,
    Ptr {
        k_mut: KMut,
        ty: Box<KTy>,
    },
    Fn {
        param_tys: Vec<KTy>,
        result_ty: Box<KTy>,
    },
    Enum(KEnum),
    Struct(KStruct),
}

impl KTy {
    pub(crate) fn is_unresolved(&self) -> bool {
        match self {
            KTy::Unresolved => true,
            _ => false,
        }
    }

    pub(crate) fn is_unit(&self) -> bool {
        match self {
            KTy::Unit => true,
            _ => false,
        }
    }

    pub(crate) fn is_primitive(&self) -> bool {
        match self {
            KTy::I8
            | KTy::I16
            | KTy::I32
            | KTy::I64
            | KTy::Isize
            | KTy::U8
            | KTy::U16
            | KTy::U32
            | KTy::U64
            | KTy::Usize
            | KTy::F32
            | KTy::F64
            | KTy::C8
            | KTy::C16
            | KTy::C32
            | KTy::Bool
            | KTy::Ptr { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn into_ptr(self, k_mut: KMut) -> KTy {
        KTy::Ptr {
            k_mut,
            ty: Box::new(self),
        }
    }

    pub(crate) fn as_struct(self) -> Option<KStruct> {
        match self {
            KTy::Struct(k_struct) => Some(k_struct),
            _ => None,
        }
    }
}

impl Default for KTy {
    fn default() -> Self {
        KTy::Unresolved
    }
}

impl Debug for KTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KTy::Unresolved => write!(f, "???"),
            KTy::Meta(meta) => Debug::fmt(meta, f),
            KTy::Never => write!(f, "never"),
            KTy::Unit => write!(f, "()"),
            KTy::Bool => write!(f, "bool"),
            KTy::I8 => write!(f, "i8"),
            KTy::I16 => write!(f, "i16"),
            KTy::I32 => write!(f, "i32"),
            KTy::I64 => write!(f, "i64"),
            KTy::Isize => write!(f, "isize"),
            KTy::U8 => write!(f, "u8"),
            KTy::U16 => write!(f, "u16"),
            KTy::U32 => write!(f, "u32"),
            KTy::U64 => write!(f, "u64"),
            KTy::Usize => write!(f, "usize"),
            KTy::F32 => write!(f, "f32"),
            KTy::F64 => write!(f, "f64"),
            KTy::C8 => write!(f, "c8"),
            KTy::C16 => write!(f, "c16"),
            KTy::C32 => write!(f, "c32"),
            KTy::Ptr { k_mut, ty } => {
                write!(f, "*")?;
                if let KMut::Mut = k_mut {
                    write!(f, "mut ")?;
                }
                Debug::fmt(&ty, f)
            }
            KTy::Fn {
                param_tys,
                result_ty,
            } => {
                write!(f, "fn(")?;
                for (i, _ty) in param_tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    // NOTE: this cause stack overflow
                    // Debug::fmt(ty, f)?;
                    write!(f, "_")?;
                }
                write!(f, ") -> ")?;
                Debug::fmt(result_ty, f)
            }
            KTy::Enum(k_enum) => {
                // FIXME: print name
                write!(f, "enum#{}", k_enum.id())
            }
            KTy::Struct(k_struct) => {
                // FIXME: print name
                write!(f, "struct {}", k_struct.id())
            }
        }
    }
}
