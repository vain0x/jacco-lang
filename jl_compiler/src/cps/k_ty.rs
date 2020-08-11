use super::{KAlias, KEnum, KMetaTy, KMod, KModOutlines, KMut, KStruct, KTyEnv};
use crate::{
    parse::ATyId,
    source::Loc,
    utils::{DebugWith, DebugWithContext},
};
use std::{
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    ops::Deref,
};

pub(crate) enum KEnumOrStruct {
    Enum(KMod, KEnum),
    Struct(KMod, KStruct),
}

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

/// 型 (その2)
///
/// 目的: 単一化の実装を簡略化すること、メタ型変数を含むこと、他のモジュールの enum/struct の型を表現すること
#[derive(Clone, Eq, PartialEq, Hash)]
pub(crate) enum KTy2 {
    Unresolved {
        cause: KTyCause,
    },
    Meta(KMetaTy),
    Never,
    Unit,
    Number(KNumberTy),
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
    pub(crate) const DEFAULT: KTy2 = KTy2::Unresolved {
        cause: KTyCause::Default,
    };
    #[allow(unused)]
    pub(crate) const I8: KTy2 = KTy2::Number(KNumberTy::I8);
    #[allow(unused)]
    pub(crate) const I16: KTy2 = KTy2::Number(KNumberTy::I16);
    pub(crate) const I32: KTy2 = KTy2::Number(KNumberTy::I32);
    pub(crate) const I64: KTy2 = KTy2::Number(KNumberTy::I64);
    pub(crate) const ISIZE: KTy2 = KTy2::Number(KNumberTy::Isize);
    #[allow(unused)]
    pub(crate) const INN: KTy2 = KTy2::Number(KNumberTy::INN);
    #[allow(unused)]
    pub(crate) const U8: KTy2 = KTy2::Number(KNumberTy::U8);
    #[allow(unused)]
    pub(crate) const U16: KTy2 = KTy2::Number(KNumberTy::U16);
    #[allow(unused)]
    pub(crate) const U32: KTy2 = KTy2::Number(KNumberTy::U32);
    pub(crate) const U64: KTy2 = KTy2::Number(KNumberTy::U64);
    pub(crate) const USIZE: KTy2 = KTy2::Number(KNumberTy::Usize);
    #[allow(unused)]
    pub(crate) const UNN: KTy2 = KTy2::Number(KNumberTy::UNN);
    #[allow(unused)]
    pub(crate) const F32: KTy2 = KTy2::Number(KNumberTy::F32);
    pub(crate) const F64: KTy2 = KTy2::Number(KNumberTy::F64);
    #[allow(unused)]
    pub(crate) const FNN: KTy2 = KTy2::Number(KNumberTy::FNN);
    pub(crate) const C8: KTy2 = KTy2::Number(KNumberTy::C8);
    #[allow(unused)]
    pub(crate) const C16: KTy2 = KTy2::Number(KNumberTy::C16);
    #[allow(unused)]
    pub(crate) const C32: KTy2 = KTy2::Number(KNumberTy::C32);
    #[allow(unused)]
    pub(crate) const CNN: KTy2 = KTy2::Number(KNumberTy::CNN);
    pub(crate) const BOOL: KTy2 = KTy2::Number(KNumberTy::Bool);

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
            KTy::Unresolved { cause } => KTy2::Unresolved { cause },
            KTy::Never => KTy2::Never,
            KTy::Unit => KTy2::Unit,
            KTy::Number(number_ty) => KTy2::Number(number_ty),
            KTy::Ptr { k_mut, ty } => KTy2::from_ty1(*ty, k_mod).into_ptr(k_mut),
            KTy::Fn {
                param_tys,
                result_ty,
            } => KTy2::new_fn(
                param_tys.into_iter().map(|ty| KTy2::from_ty1(ty, k_mod)),
                KTy2::from_ty1(*result_ty, k_mod),
            ),
            KTy::Alias(_) => KTy2::Unresolved {
                cause: KTyCause::Alias,
            },
            KTy::Enum(k_enum) => KTy2::new_enum(k_mod, k_enum),
            KTy::Struct(k_struct) => KTy2::new_struct(k_mod, k_struct),
        }
    }

    pub(crate) fn is_unbound(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Unresolved { .. } => true,
            _ => false,
        })
    }

    pub(crate) fn is_unresolved(&self) -> bool {
        match *self {
            KTy2::Unresolved { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn is_unit(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Unit => true,
            _ => false,
        })
    }

    pub(crate) fn is_unit_or_never(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Unresolved { .. } | KTy2::Never | KTy2::Unit => true,
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
            KTy2::Number(_) => true,
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

    pub(crate) fn display(&self, ty_env: &KTyEnv, mod_outlines: &KModOutlines) -> String {
        format!("{:?}", DebugWith::new(self, &(ty_env, mod_outlines)))
    }
}

impl Default for KTy2 {
    fn default() -> Self {
        KTy2::Unresolved {
            cause: KTyCause::Default,
        }
    }
}

impl<'a> DebugWithContext<(&'a KTyEnv, &'a KModOutlines)> for KTy2 {
    fn fmt(&self, context: &(&'a KTyEnv, &'a KModOutlines), f: &mut Formatter<'_>) -> fmt::Result {
        let (ty_env, mod_outlines) = context;

        match self {
            KTy2::Unresolved { cause } => write!(f, "{{unresolved}} ?{:?}", cause),
            KTy2::Meta(meta_ty) => match meta_ty.try_unwrap(ty_env) {
                Some(ty) => DebugWithContext::fmt(ty.borrow().deref(), context, f),
                None => write!(f, "?{}", meta_ty.to_index()),
            },
            KTy2::Never => write!(f, "!"),
            KTy2::Unit => write!(f, "()"),
            KTy2::Number(number_ty) => write!(f, "{}", number_ty.as_str()),
            KTy2::Ptr { k_mut, base_ty } => {
                match k_mut {
                    KMut::Const => write!(f, "*")?,
                    KMut::Mut => write!(f, "*mut ")?,
                }
                DebugWithContext::fmt(base_ty.deref(), context, f)
            }
            KTy2::Fn {
                param_tys,
                result_ty,
            } => {
                let mut tuple = f.debug_tuple("fn");
                for param_ty in param_tys {
                    tuple.field(&DebugWith::new(param_ty, context));
                }
                tuple.finish()?;

                if !result_ty.is_unit(ty_env) {
                    write!(f, " -> ")?;
                    DebugWithContext::fmt(result_ty.deref(), context, f)?;
                }
                Ok(())
            }
            KTy2::Enum(k_mod, k_enum) => write!(
                f,
                "enum {}::{}",
                k_mod.of(mod_outlines).name,
                k_enum.of(&k_mod.of(mod_outlines).enums).name
            ),
            KTy2::Struct(k_mod, k_struct) => write!(
                f,
                "struct {}::{}",
                k_mod.of(mod_outlines).name,
                k_struct.of(&k_mod.of(mod_outlines).structs).name
            ),
        }
    }
}

impl Debug for KTy2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KTy2::Unresolved { cause } => write!(f, "{{unresolved}} ?{:?}", cause),
            KTy2::Meta(meta_ty) => write!(f, "meta#{}", meta_ty.to_index()),
            KTy2::Never => write!(f, "!"),
            KTy2::Unit => write!(f, "()"),
            KTy2::Number(number_ty) => write!(f, "{}", number_ty.as_str()),
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
            None => f(&KTy2::Unresolved {
                cause: KTyCause::Unbound,
            }),
        },
        _ => f(ty),
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum KTyCause {
    Default,
    InitLater(Loc),
    Miss,
    Alias,
    FieldTag,
    NameUnresolved(ATyId),
    InferTy(ATyId),
    Unbound,
    Loc(Loc),
}

impl PartialEq for KTyCause {
    fn eq(&self, _: &KTyCause) -> bool {
        true
    }
}

impl Eq for KTyCause {}

impl Hash for KTyCause {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

#[derive(Clone, PartialEq)]
pub(crate) enum KTy {
    Unresolved {
        cause: KTyCause,
    },
    Never,
    Unit,
    Number(KNumberTy),
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
    #[allow(unused)]
    pub(crate) const DEFAULT: KTy = KTy::Unresolved {
        cause: KTyCause::Default,
    };
    pub(crate) const BOOL: KTy = KTy::Number(KNumberTy::Bool);
    pub(crate) const I8: KTy = KTy::Number(KNumberTy::I8);
    pub(crate) const I16: KTy = KTy::Number(KNumberTy::I16);
    pub(crate) const I32: KTy = KTy::Number(KNumberTy::I32);
    pub(crate) const I64: KTy = KTy::Number(KNumberTy::I64);
    pub(crate) const ISIZE: KTy = KTy::Number(KNumberTy::Isize);
    #[allow(unused)]
    pub(crate) const INN: KTy = KTy::Number(KNumberTy::INN);
    pub(crate) const U8: KTy = KTy::Number(KNumberTy::U8);
    pub(crate) const U16: KTy = KTy::Number(KNumberTy::U16);
    pub(crate) const U32: KTy = KTy::Number(KNumberTy::U32);
    pub(crate) const U64: KTy = KTy::Number(KNumberTy::U64);
    pub(crate) const USIZE: KTy = KTy::Number(KNumberTy::Usize);
    #[allow(unused)]
    pub(crate) const UNN: KTy = KTy::Number(KNumberTy::UNN);
    pub(crate) const F32: KTy = KTy::Number(KNumberTy::F32);
    pub(crate) const F64: KTy = KTy::Number(KNumberTy::F64);
    #[allow(unused)]
    pub(crate) const FNN: KTy = KTy::Number(KNumberTy::FNN);
    pub(crate) const C8: KTy = KTy::Number(KNumberTy::C8);
    pub(crate) const C16: KTy = KTy::Number(KNumberTy::C16);
    pub(crate) const C32: KTy = KTy::Number(KNumberTy::C32);
    #[allow(unused)]
    pub(crate) const CNN: KTy = KTy::Number(KNumberTy::CNN);

    pub(crate) fn init_later(loc: Loc) -> Self {
        KTy::Unresolved {
            cause: KTyCause::InitLater(loc),
        }
    }

    #[allow(unused)]
    pub(crate) fn from_number_ty(number_ty: KNumberTy) -> &'static KTy {
        match number_ty {
            KNumberTy::Bool => &KTy::BOOL,
            KNumberTy::I8 => &KTy::I8,
            KNumberTy::I16 => &KTy::I16,
            KNumberTy::I32 => &KTy::I32,
            KNumberTy::I64 => &KTy::I64,
            KNumberTy::Isize => &KTy::ISIZE,
            KNumberTy::INN => &KTy::INN,
            KNumberTy::U8 => &KTy::U8,
            KNumberTy::U16 => &KTy::U16,
            KNumberTy::U32 => &KTy::U32,
            KNumberTy::U64 => &KTy::U64,
            KNumberTy::Usize => &KTy::USIZE,
            KNumberTy::UNN => &KTy::UNN,
            KNumberTy::F32 => &KTy::F32,
            KNumberTy::F64 => &KTy::F64,
            KNumberTy::FNN => &KTy::FNN,
            KNumberTy::C8 => &KTy::C8,
            KNumberTy::C16 => &KTy::C16,
            KNumberTy::C32 => &KTy::C32,
            KNumberTy::CNN => &KTy::CNN,
        }
    }

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
            KTy::Unresolved { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn is_unit(&self) -> bool {
        match self {
            KTy::Unit => true,
            _ => false,
        }
    }

    pub(crate) fn as_struct(&self) -> Option<KStruct> {
        match self {
            KTy::Struct(k_struct) => Some(*k_struct),
            _ => None,
        }
    }
}

impl Default for KTy {
    fn default() -> Self {
        KTy::Unresolved {
            cause: KTyCause::Default,
        }
    }
}

impl Debug for KTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KTy::Unresolved { .. } => write!(f, "???"),
            KTy::Never => write!(f, "never"),
            KTy::Unit => write!(f, "()"),
            KTy::Number(number_ty) => write!(f, "{}", number_ty.as_str()),
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
