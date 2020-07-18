use super::{KAlias, KEnum, KMetaTy, KMod, KMut, KStruct, KTyEnv};
use std::{
    fmt::{self, Debug, Formatter},
    iter::once,
    mem::take,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum KBasicTy {
    Unit,
    I8,
    I16,
    I32,
    I64,
    Isize,
    #[allow(unused)]
    IConst,
    U8,
    U16,
    U32,
    U64,
    Usize,
    #[allow(unused)]
    UConst,
    F32,
    F64,
    #[allow(unused)]
    FConst,
    C8,
    C16,
    C32,
    #[allow(unused)]
    CConst,
    Bool,
}

impl KBasicTy {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            KBasicTy::Unit => "()",
            KBasicTy::I8 => "i8",
            KBasicTy::I16 => "i16",
            KBasicTy::I32 => "i32",
            KBasicTy::I64 => "i64",
            KBasicTy::Isize => "isize",
            KBasicTy::IConst => "{iNN}",
            KBasicTy::U8 => "u8",
            KBasicTy::U16 => "u16",
            KBasicTy::U32 => "u32",
            KBasicTy::U64 => "u64",
            KBasicTy::Usize => "usize",
            KBasicTy::UConst => "{uNN}",
            KBasicTy::F32 => "f32",
            KBasicTy::F64 => "f64",
            KBasicTy::FConst => "{fNN}",
            KBasicTy::C8 => "c8",
            KBasicTy::C16 => "c16",
            KBasicTy::C32 => "c32",
            KBasicTy::CConst => "{cNN}",
            KBasicTy::Bool => "bool",
        }
    }
}

impl Debug for KBasicTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// 型コンストラクタ
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KTyCtor {
    Ptr(KMut),
    Fn,
    Enum(KMod, KEnum),
    Struct(KMod, KStruct),
}

/// 型 (その2)
#[derive(Clone, Eq, PartialEq, Hash)]
pub(crate) enum KTy2 {
    Unresolved,
    Never,
    Meta(KMetaTy),
    Basic(KBasicTy),
    App { ctor: KTyCtor, args: Box<[KTy2]> },
}

impl KTy2 {
    pub(crate) fn into_ptr(self, k_mut: KMut) -> KTy2 {
        KTy2::App {
            ctor: KTyCtor::Ptr(k_mut),
            args: Box::new([self]),
        }
    }

    pub(crate) fn new_fn(param_tys: impl IntoIterator<Item = KTy2>, result_ty: KTy2) -> KTy2 {
        KTy2::App {
            ctor: KTyCtor::Fn,
            args: param_tys.into_iter().chain(once(result_ty)).collect(),
        }
    }

    pub(crate) fn new_enum(k_mod: KMod, k_enum: KEnum) -> KTy2 {
        KTy2::App {
            ctor: KTyCtor::Enum(k_mod, k_enum),
            args: Box::default(),
        }
    }

    pub(crate) fn new_struct(k_mod: KMod, k_struct: KStruct) -> KTy2 {
        KTy2::App {
            ctor: KTyCtor::Struct(k_mod, k_struct),
            args: Box::default(),
        }
    }

    pub(crate) fn from_ty1(ty: KTy, k_mod: KMod) -> Self {
        match ty {
            KTy::Unresolved => KTy2::Unresolved,
            KTy::Meta(meta_ty) => KTy2::Meta(meta_ty),
            KTy::Never => KTy2::Never,
            KTy::Unit => KTy2::Basic(KBasicTy::Unit),
            KTy::I8 => KTy2::Basic(KBasicTy::I8),
            KTy::I16 => KTy2::Basic(KBasicTy::I16),
            KTy::I32 => KTy2::Basic(KBasicTy::I32),
            KTy::I64 => KTy2::Basic(KBasicTy::I64),
            KTy::Isize => KTy2::Basic(KBasicTy::Isize),
            KTy::U8 => KTy2::Basic(KBasicTy::U8),
            KTy::U16 => KTy2::Basic(KBasicTy::U16),
            KTy::U32 => KTy2::Basic(KBasicTy::U32),
            KTy::U64 => KTy2::Basic(KBasicTy::U64),
            KTy::Usize => KTy2::Basic(KBasicTy::Usize),
            KTy::F32 => KTy2::Basic(KBasicTy::F32),
            KTy::F64 => KTy2::Basic(KBasicTy::F64),
            KTy::C8 => KTy2::Basic(KBasicTy::C8),
            KTy::C16 => KTy2::Basic(KBasicTy::C16),
            KTy::C32 => KTy2::Basic(KBasicTy::C32),
            KTy::Bool => KTy2::Basic(KBasicTy::Bool),
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

    pub(crate) fn to_ty1(&self) -> KTy {
        match self.clone() {
            KTy2::Unresolved => KTy::Unresolved,
            KTy2::Never => KTy::Never,
            KTy2::Meta(meta_ty) => KTy::Meta(meta_ty),
            KTy2::Basic(basic_ty) => match basic_ty {
                KBasicTy::Unit => KTy::Unit,
                KBasicTy::I8 => KTy::I8,
                KBasicTy::I16 => KTy::I16,
                KBasicTy::I32 => KTy::I32,
                KBasicTy::I64 => KTy::I64,
                KBasicTy::Isize => KTy::Isize,
                KBasicTy::U8 => KTy::U8,
                KBasicTy::U16 => KTy::U16,
                KBasicTy::U32 => KTy::U32,
                KBasicTy::U64 => KTy::U64,
                KBasicTy::Usize => KTy::Usize,
                KBasicTy::F32 => KTy::F32,
                KBasicTy::F64 => KTy::F64,
                KBasicTy::C8 => KTy::C8,
                KBasicTy::C16 => KTy::C16,
                KBasicTy::C32 => KTy::C32,
                KBasicTy::Bool => KTy::Bool,
                KBasicTy::IConst => KTy::I32,
                KBasicTy::UConst => KTy::Usize,
                KBasicTy::FConst => KTy::F64,
                KBasicTy::CConst => KTy::C8,
            },
            KTy2::App { ctor, mut args } => match ctor {
                KTyCtor::Ptr(k_mut) => match &mut *args {
                    [ty] => KTy::Ptr {
                        k_mut,
                        ty: Box::new(take(ty).to_ty1()),
                    },
                    _ => KTy::Unresolved,
                },
                KTyCtor::Fn => match &mut *args {
                    [] => KTy::Unresolved,
                    [param_tys @ .., result_ty] => KTy::Fn {
                        param_tys: param_tys
                            .iter_mut()
                            .map(|param_ty| take(param_ty).to_ty1())
                            .collect(),
                        result_ty: Box::new(take(result_ty).to_ty1()),
                    },
                },
                KTyCtor::Enum(_, k_enum) => KTy::Enum(k_enum),
                KTyCtor::Struct(_, k_struct) => KTy::Struct(k_struct),
            },
        }
    }

    /// 型が一定の条件を満たすか判定する。
    /// 束縛済みのメタ型変数は自動で展開されるので、`predicate` に `KTy2::Meta` が渡されることはない。
    fn satisfies(&self, ty_env: &KTyEnv, predicate: impl Fn(&KTy2) -> bool) -> bool {
        match self {
            KTy2::Meta(meta_ty) => match meta_ty.try_unwrap(ty_env) {
                Some(ty) => ty.borrow().satisfies(ty_env, predicate),
                None => predicate(&KTy2::Unresolved),
            },
            _ => predicate(self),
        }
    }

    pub(crate) fn is_unbound(&self, ty_env: &KTyEnv) -> bool {
        self.satisfies(ty_env, |ty| match ty {
            KTy2::Unresolved => true,
            _ => false,
        })
    }

    pub(crate) fn is_unresolved(&self) -> bool {
        *self == KTy2::Unresolved
    }

    pub(crate) fn is_unit(&self, ty_env: &KTyEnv) -> bool {
        self.satisfies(ty_env, |ty| match ty {
            KTy2::Basic(KBasicTy::Unit) => true,
            _ => false,
        })
    }

    pub(crate) fn is_unit_or_never(&self, ty_env: &KTyEnv) -> bool {
        self.satisfies(ty_env, |ty| match ty {
            KTy2::Unresolved | KTy2::Never | KTy2::Basic(KBasicTy::Unit) => true,
            _ => false,
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
        fn write_ty_app(ctor: &KTyCtor, args: &[KTy2], f: &mut Formatter<'_>) -> fmt::Result {
            static mut DEPTH: usize = 0;
            let depth = unsafe { DEPTH };
            if depth >= 3 {
                return write!(f, "_");
            }

            unsafe { DEPTH = depth + 1 };
            {
                Debug::fmt(ctor, f)?;
                f.debug_list().entries(args).finish()?;
            }
            unsafe { DEPTH = depth };
            Ok(())
        }

        match self {
            KTy2::Unresolved => write!(f, "{{unresolved}}"),
            KTy2::Never => write!(f, "!"),
            KTy2::Meta(meta_ty) => write!(f, "meta#{}", meta_ty.to_index()),
            KTy2::Basic(basic_ty) => write!(f, "{}", basic_ty.as_str()),
            KTy2::App { ctor, args } => match ctor {
                KTyCtor::Ptr(k_mut) => match &**args {
                    [base_ty] => {
                        match k_mut {
                            KMut::Const => write!(f, "*")?,
                            KMut::Mut => write!(f, "*mut ")?,
                        }
                        Debug::fmt(base_ty, f)
                    }
                    _ => write_ty_app(ctor, args, f),
                },
                // FIXME: 実装
                KTyCtor::Fn => write_ty_app(ctor, args, f),
                KTyCtor::Enum(_, _) => write_ty_app(ctor, args, f),
                KTyCtor::Struct(_, _) => write_ty_app(ctor, args, f),
            },
        }
    }
}

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
    Alias(KAlias),
    Enum(KEnum),
    Struct(KStruct),
}

impl KTy {
    pub(crate) fn to_ty2(&self, k_mod: KMod) -> KTy2 {
        KTy2::from_ty1(self.clone(), k_mod)
    }

    /// 型が一定の条件を満たすか判定する。
    /// 束縛済みのメタ型変数は自動で展開されるので、`predicate` に `KTy::Meta` が渡されることはない。
    fn satisfies(&self, ty_env: &KTyEnv, predicate: impl Fn(&KTy) -> bool) -> bool {
        match self {
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(ty_env) {
                Some(ty) => ty.borrow().to_ty1().satisfies(ty_env, predicate),
                None => predicate(&KTy::Unresolved),
            },
            _ => predicate(self),
        }
    }

    pub(crate) fn is_unresolved(&self) -> bool {
        match self {
            KTy::Unresolved => true,
            _ => false,
        }
    }

    pub(crate) fn is_unit(&self, ty_env: &KTyEnv) -> bool {
        self.satisfies(ty_env, |ty| match ty {
            KTy::Unit => true,
            _ => false,
        })
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

    pub(crate) fn as_struct(&self) -> Option<KStruct> {
        match self {
            KTy::Struct(k_struct) => Some(*k_struct),
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
#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum Variance {
    /// Invariant. 不変。アップキャストは許可されない。
    In,
    /// Covariant. 共変。アップキャストが許可される。
    Co,
    /// Contravariant. 反変。逆方向のアップキャストが許可される。
    Contra,
}

/// 型引数の変性を取得する。
pub(crate) fn ty_arg_variance(ctor: &KTyCtor, i: usize, len: usize) -> Variance {
    match ctor {
        KTyCtor::Ptr(KMut::Const) => Variance::Co,
        KTyCtor::Ptr(KMut::Mut) => Variance::In,
        KTyCtor::Fn => {
            if i + 1 < len {
                Variance::Co
            } else {
                Variance::Contra
            }
        }
        KTyCtor::Enum(_, _) | KTyCtor::Struct(_, _) => Variance::In,
    }
}
