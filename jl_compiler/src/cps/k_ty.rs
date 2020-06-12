use super::{KMetaTy, KStruct};
use std::fmt::{self, Debug, Formatter};

#[derive(Clone)]
pub(crate) enum KTy {
    Unresolved,
    Meta(KMetaTy),
    Never,
    Unit,
    I32,
    Ptr {
        ty: Box<KTy>,
    },
    Fn {
        param_tys: Vec<KTy>,
        result_ty: Box<KTy>,
    },
    Struct(KStruct),
}

impl KTy {
    pub(crate) fn is_unresolved(&self) -> bool {
        match self {
            KTy::Unresolved => true,
            _ => false,
        }
    }

    pub(crate) fn is_struct(&self) -> bool {
        match self {
            KTy::Struct { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn as_ptr(self) -> Option<KTy> {
        match self {
            KTy::Ptr { ty } => Some(*ty),
            _ => None,
        }
    }

    pub(crate) fn into_ptr(self) -> KTy {
        KTy::Ptr { ty: Box::new(self) }
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
            KTy::I32 => write!(f, "i32"),
            KTy::Ptr { ty } => {
                write!(f, "*")?;
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
                    // fmt::Debug::fmt(ty, f)?;
                    write!(f, "_")?;
                }
                write!(f, ") -> ")?;
                fmt::Debug::fmt(result_ty, f)
            }
            KTy::Struct(k_struct) => {
                // FIXME: print name
                write!(f, "struct {}", k_struct.id())
            }
        }
    }
}
