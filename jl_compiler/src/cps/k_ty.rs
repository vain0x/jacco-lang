use super::{KAlias, KEnum, KMetaTy, KMod, KMut, KStruct, KTyEnv};
use std::fmt::{self, Debug, Formatter};

pub(crate) enum KEnumOrStruct {
    Enum(KMod, KEnum),
    Struct(KMod, KStruct),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum KBasicTy {
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
    Bool,
}

impl KBasicTy {
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            KBasicTy::I8 => "i8",
            KBasicTy::I16 => "i16",
            KBasicTy::I32 => "i32",
            KBasicTy::I64 => "i64",
            KBasicTy::Isize => "isize",
            KBasicTy::INN => "{iNN}",
            KBasicTy::U8 => "u8",
            KBasicTy::U16 => "u16",
            KBasicTy::U32 => "u32",
            KBasicTy::U64 => "u64",
            KBasicTy::Usize => "usize",
            KBasicTy::UNN => "{uNN}",
            KBasicTy::F32 => "f32",
            KBasicTy::F64 => "f64",
            KBasicTy::FNN => "{fNN}",
            KBasicTy::C8 => "c8",
            KBasicTy::C16 => "c16",
            KBasicTy::C32 => "c32",
            KBasicTy::CNN => "{cNN}",
            KBasicTy::Bool => "bool",
        }
    }
}

impl Debug for KBasicTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// 型 (その2)
///
/// 目的: 単一化の実装を簡略化すること、メタ型変数を含むこと、他のモジュールの enum/struct の型を表現すること
#[derive(Clone, Eq, PartialEq, Hash)]
pub(crate) enum KTy2 {
    Unresolved,
    Meta(KMetaTy),
    Never,
    Unit,
    Basic(KBasicTy),
    Ptr {
        k_mut: KMut,
        base_ty: Box<KTy2>,
    },
    Fn {
        param_tys: Vec<KTy2>,
        result_ty: Box<KTy2>,
    },
    Enum(KMod, KEnum),
    Struct(KMod, KStruct),
}

impl KTy2 {
    #[allow(unused)]
    pub(crate) const I8: KTy2 = KTy2::Basic(KBasicTy::I8);
    #[allow(unused)]
    pub(crate) const I16: KTy2 = KTy2::Basic(KBasicTy::I16);
    pub(crate) const I32: KTy2 = KTy2::Basic(KBasicTy::I32);
    pub(crate) const I64: KTy2 = KTy2::Basic(KBasicTy::I64);
    pub(crate) const ISIZE: KTy2 = KTy2::Basic(KBasicTy::Isize);
    #[allow(unused)]
    pub(crate) const INN: KTy2 = KTy2::Basic(KBasicTy::INN);
    #[allow(unused)]
    pub(crate) const U8: KTy2 = KTy2::Basic(KBasicTy::U8);
    #[allow(unused)]
    pub(crate) const U16: KTy2 = KTy2::Basic(KBasicTy::U16);
    #[allow(unused)]
    pub(crate) const U32: KTy2 = KTy2::Basic(KBasicTy::U32);
    pub(crate) const U64: KTy2 = KTy2::Basic(KBasicTy::U64);
    pub(crate) const USIZE: KTy2 = KTy2::Basic(KBasicTy::Usize);
    #[allow(unused)]
    pub(crate) const UNN: KTy2 = KTy2::Basic(KBasicTy::UNN);
    #[allow(unused)]
    pub(crate) const F32: KTy2 = KTy2::Basic(KBasicTy::F32);
    pub(crate) const F64: KTy2 = KTy2::Basic(KBasicTy::F64);
    #[allow(unused)]
    pub(crate) const FNN: KTy2 = KTy2::Basic(KBasicTy::FNN);
    pub(crate) const C8: KTy2 = KTy2::Basic(KBasicTy::C8);
    #[allow(unused)]
    pub(crate) const C16: KTy2 = KTy2::Basic(KBasicTy::C16);
    #[allow(unused)]
    pub(crate) const C32: KTy2 = KTy2::Basic(KBasicTy::C32);
    #[allow(unused)]
    pub(crate) const CNN: KTy2 = KTy2::Basic(KBasicTy::CNN);
    pub(crate) const BOOL: KTy2 = KTy2::Basic(KBasicTy::Bool);

    pub(crate) fn into_ptr(self, k_mut: KMut) -> KTy2 {
        KTy2::Ptr {
            k_mut,
            base_ty: Box::new(self),
        }
    }

    pub(crate) fn new_fn(param_tys: impl IntoIterator<Item = KTy2>, result_ty: KTy2) -> KTy2 {
        KTy2::Fn {
            param_tys: param_tys.into_iter().collect(),
            result_ty: Box::new(result_ty),
        }
    }

    pub(crate) fn new_enum(k_mod: KMod, k_enum: KEnum) -> KTy2 {
        KTy2::Enum(k_mod, k_enum)
    }

    pub(crate) fn new_struct(k_mod: KMod, k_struct: KStruct) -> KTy2 {
        KTy2::Struct(k_mod, k_struct)
    }

    pub(crate) fn from_ty1(ty: KTy, k_mod: KMod) -> Self {
        match ty {
            KTy::Unresolved => KTy2::Unresolved,
            KTy::Never => KTy2::Never,
            KTy::Unit => KTy2::Unit,
            KTy::Basic(basic_ty) => KTy2::Basic(basic_ty),
            KTy::Ptr { k_mut, ty } => KTy2::from_ty1(*ty, k_mod).into_ptr(k_mut),
            KTy::Fn {
                param_tys,
                result_ty,
            } => KTy2::new_fn(
                param_tys.into_iter().map(|ty| KTy2::from_ty1(ty, k_mod)),
                KTy2::from_ty1(*result_ty, k_mod),
            ),
            KTy::Alias(_) => KTy2::Unresolved,
            KTy::Enum(k_enum) => KTy2::new_enum(k_mod, k_enum),
            KTy::Struct(k_struct) => KTy2::new_struct(k_mod, k_struct),
        }
    }

    pub(crate) fn is_unbound(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Unresolved => true,
            _ => false,
        })
    }

    pub(crate) fn is_unresolved(&self) -> bool {
        *self == KTy2::Unresolved
    }

    pub(crate) fn is_unit(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Unit => true,
            _ => false,
        })
    }

    pub(crate) fn is_unit_or_never(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Unresolved | KTy2::Never | KTy2::Unit => true,
            _ => false,
        })
    }

    pub(crate) fn is_bool(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::BOOL => true,
            _ => false,
        })
    }

    // FIXME: const enum も primitive とみなす
    pub(crate) fn is_primitive(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Basic(_) => true,
            KTy2::Ptr { .. } => true,
            _ => false,
        })
    }

    pub(crate) fn is_ptr(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Ptr { .. } => true,
            _ => false,
        })
    }

    pub(crate) fn as_ptr(&self, ty_env: &KTyEnv) -> Option<(KMut, KTy2)> {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Ptr { k_mut, base_ty } => Some((*k_mut, (**base_ty).clone())),
            _ => None,
        })
    }

    pub(crate) fn as_fn(&self, ty_env: &KTyEnv) -> Option<(Vec<KTy2>, KTy2)> {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Fn {
                param_tys,
                result_ty,
            } => Some((param_tys.to_owned(), (**result_ty).clone())),
            _ => None,
        })
    }

    pub(crate) fn is_struct_or_enum(&self, ty_env: &KTyEnv) -> bool {
        self.as_struct_or_enum(ty_env).is_some()
    }

    pub(crate) fn as_struct_or_enum(&self, ty_env: &KTyEnv) -> Option<KEnumOrStruct> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Enum(k_mod, k_enum) => Some(KEnumOrStruct::Enum(k_mod, k_enum)),
            KTy2::Struct(k_mod, k_struct) => Some(KEnumOrStruct::Struct(k_mod, k_struct)),
            _ => None,
        })
    }

    pub(crate) fn as_enum(&self, ty_env: &KTyEnv) -> Option<(KMod, KEnum)> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Enum(k_mod, k_enum) => Some((k_mod, k_enum)),
            _ => None,
        })
    }

    #[allow(unused)]
    pub(crate) fn as_struct(&self, ty_env: &KTyEnv) -> Option<(KMod, KStruct)> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Struct(k_mod, k_struct) => Some((k_mod, k_struct)),
            _ => None,
        })
    }

    pub(crate) fn display(&self, _ty_env: &KTyEnv) -> String {
        format!("{:?}", self)
    }
}

impl Default for KTy2 {
    fn default() -> Self {
        KTy2::Unresolved
    }
}

impl Debug for KTy2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KTy2::Unresolved => write!(f, "{{unresolved}}"),
            KTy2::Meta(meta_ty) => write!(f, "meta#{}", meta_ty.to_index()),
            KTy2::Never => write!(f, "!"),
            KTy2::Unit => write!(f, "()"),
            KTy2::Basic(basic_ty) => write!(f, "{}", basic_ty.as_str()),
            KTy2::Ptr { k_mut, base_ty } => {
                match k_mut {
                    KMut::Const => write!(f, "*")?,
                    KMut::Mut => write!(f, "*mut ")?,
                }
                Debug::fmt(base_ty, f)
            }
            // FIXME: 実装
            KTy2::Fn { .. } => Ok(()),
            KTy2::Enum(_, _) => Ok(()),
            KTy2::Struct(_, _) => Ok(()),
        }
    }
}

/// NOTE: メタ型変数は自動で展開されるので、`f` に `KTy::Meta` が渡されることはない。
fn ty2_map<T>(ty: &KTy2, ty_env: &KTyEnv, f: impl Fn(&KTy2) -> T) -> T {
    match ty {
        KTy2::Meta(meta_ty) => match meta_ty.try_unwrap(ty_env) {
            Some(ty) => ty2_map(&ty.borrow(), ty_env, f),
            None => f(&KTy2::Unresolved),
        },
        _ => f(ty),
    }
}

#[derive(Clone, Eq, PartialEq)]
pub(crate) enum KTy {
    Unresolved,
    Never,
    Unit,
    Basic(KBasicTy),
    Ptr {
        k_mut: KMut,
        ty: Box<KTy>,
    },
    Fn {
        param_tys: Vec<KTy>,
        result_ty: Box<KTy>,
    },
    Alias(KAlias),
    Enum(KEnum),
    Struct(KStruct),
}

impl KTy {
    pub(crate) const I8: KTy = KTy::Basic(KBasicTy::I8);
    pub(crate) const I16: KTy = KTy::Basic(KBasicTy::I16);
    pub(crate) const I32: KTy = KTy::Basic(KBasicTy::I32);
    pub(crate) const I64: KTy = KTy::Basic(KBasicTy::I64);
    pub(crate) const ISIZE: KTy = KTy::Basic(KBasicTy::Isize);
    pub(crate) const U8: KTy = KTy::Basic(KBasicTy::U8);
    pub(crate) const U16: KTy = KTy::Basic(KBasicTy::U16);
    pub(crate) const U32: KTy = KTy::Basic(KBasicTy::U32);
    pub(crate) const U64: KTy = KTy::Basic(KBasicTy::U64);
    pub(crate) const USIZE: KTy = KTy::Basic(KBasicTy::Usize);
    pub(crate) const F32: KTy = KTy::Basic(KBasicTy::F32);
    pub(crate) const F64: KTy = KTy::Basic(KBasicTy::F64);
    pub(crate) const C8: KTy = KTy::Basic(KBasicTy::C8);
    pub(crate) const C16: KTy = KTy::Basic(KBasicTy::C16);
    pub(crate) const C32: KTy = KTy::Basic(KBasicTy::C32);
    pub(crate) const BOOL: KTy = KTy::Basic(KBasicTy::Bool);

    pub(crate) fn to_ty2(&self, k_mod: KMod) -> KTy2 {
        KTy2::from_ty1(self.clone(), k_mod)
    }

    pub(crate) fn into_ptr(self, k_mut: KMut) -> KTy {
        KTy::Ptr {
            k_mut,
            ty: Box::new(self),
        }
    }

    pub(crate) fn is_unresolved(&self) -> bool {
        match self {
            KTy::Unresolved => true,
            _ => false,
        }
    }

    pub(crate) fn is_unit(&self, ty_env: &KTyEnv) -> bool {
        ty_map(self, ty_env, |ty| match *ty {
            KTy::Unit => true,
            _ => false,
        })
    }

    pub(crate) fn is_primitive(&self, ty_env: &KTyEnv) -> bool {
        ty_map(self, ty_env, |ty| match *ty {
            KTy::Basic(_) => true,
            KTy::Ptr { .. } => true,
            _ => false,
        })
    }

    pub(crate) fn as_struct(&self, ty_env: &KTyEnv) -> Option<KStruct> {
        ty_map(self, ty_env, |ty| match ty {
            KTy::Struct(k_struct) => Some(*k_struct),
            _ => None,
        })
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
            KTy::Never => write!(f, "never"),
            KTy::Unit => write!(f, "()"),
            KTy::Basic(basic_ty) => write!(f, "{}", basic_ty.as_str()),
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
            KTy::Alias(alias) => write!(f, "alias#{}", alias.to_index()),
            KTy::Enum(k_enum) => {
                // FIXME: print name
                write!(f, "enum#{}", k_enum.to_index())
            }
            KTy::Struct(k_struct) => {
                // FIXME: print name
                write!(f, "struct {}", k_struct.to_index())
            }
        }
    }
}

/// NOTE: 束縛済みのメタ型変数は自動で展開されるので、`f` に `KTy::Meta` が渡されることはない。
fn ty_map<T>(ty: &KTy, _ty_env: &KTyEnv, f: impl Fn(&KTy) -> T) -> T {
    f(ty)
}

/// 変性
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum Variance {
    /// Invariant. 不変。アップキャストは許可されない。
    In,
    /// Covariant. 共変。アップキャストが許可される。
    Co,
    /// Contravariant. 反変。逆方向のアップキャストが許可される。
    Contra,
}

impl Variance {
    fn inverse(self) -> Self {
        match self {
            Variance::In => Variance::In,
            Variance::Co => Variance::Contra,
            Variance::Contra => Variance::Co,
        }
    }

    pub(crate) fn reverse(&mut self) {
        let it = self.inverse();
        *self = it;
    }
}
