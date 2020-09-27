use super::*;
use crate::{
    parse::ATyId,
    source::Loc,
    utils::{DebugWith, DebugWithContext},
};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    mem::align_of,
    mem::size_of,
    mem::take,
};

pub(crate) enum KEnumOrStruct {
    Enum(KStructEnum),
    Struct(KStruct),
}

#[derive(Copy, Clone)]
struct KTy2DisplayContext<'a> {
    ty_env_opt: Option<&'a KTyEnv>,
    mod_outline_opt: Option<&'a KModOutline>,
}

/// 型 (その2)
///
/// 目的: 単一化の実装を簡略化すること、メタ型変数を含むこと、他のモジュールの enum/struct の型を表現すること
#[derive(Clone, Eq, PartialEq)]
pub(crate) enum KTy2 {
    Unresolved {
        cause: KTyCause,
    },
    /// メタ型は型変数とは異なり、型検査で一時的に発生して、単一化により他の型に束縛される。
    Meta(KMetaTy),
    /// 型変数はメタ型とは異なり、多相関数の型引数などの構文で明示的に宣言される。
    Var(KTyVar),
    Unknown,
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
    ConstEnum(KConstEnum),
    StructEnum(KStructEnum),
    Struct(KStruct),
    App {
        k_struct: KStruct,
        ty_args: HashMap<String, KTy2>,
    },
}

impl KTy2 {
    pub(crate) const TODO: KTy2 = KTy2::Unresolved {
        cause: KTyCause::Default,
    };

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

    pub(crate) fn is_never(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Never => true,
            _ => false,
        })
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

    pub(crate) fn is_primitive(&self, ty_env: &KTyEnv) -> bool {
        ty2_map(self, ty_env, |ty| match ty {
            KTy2::Number(_) => true,
            KTy2::Ptr { .. } => true,
            KTy2::ConstEnum { .. } => true,
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
            KTy2::StructEnum(struct_enum) => Some(KEnumOrStruct::Enum(struct_enum)),
            KTy2::Struct(k_struct) => Some(KEnumOrStruct::Struct(k_struct)),
            KTy2::App { k_struct, .. } => Some(KEnumOrStruct::Struct(k_struct)),
            _ => None,
        })
    }

    pub(crate) fn as_enum(&self, ty_env: &KTyEnv) -> Option<KStructEnum> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::StructEnum(struct_enum) => Some(struct_enum),
            _ => None,
        })
    }

    pub(crate) fn as_struct(&self, ty_env: &KTyEnv) -> Option<KStruct> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Struct(k_struct) => Some(k_struct),
            KTy2::App { k_struct, .. } => Some(k_struct),
            _ => None,
        })
    }

    pub(crate) fn as_struct_by_deref(&self, ty_env: &KTyEnv) -> Option<KStruct> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Struct(k_struct) => Some(k_struct),
            KTy2::Ptr { ref base_ty, .. } => base_ty.as_struct_by_deref(ty_env),
            _ => None,
        })
    }

    pub(crate) fn join(&self, other: &KTy2, ty_env: &KTyEnv) -> KTy2 {
        match (self, other) {
            (KTy2::Unresolved { .. }, ty) | (ty, KTy2::Unresolved { .. }) => ty.clone(),
            (KTy2::Unknown, _) | (_, KTy2::Unknown) => KTy2::Unknown,
            (KTy2::Never, ty) | (ty, KTy2::Never) => ty.clone(),
            (KTy2::Meta(meta_ty), other) => match meta_ty.try_unwrap(ty_env) {
                Some(ty_cell) => ty_cell.borrow().join(other, ty_env),
                None => todo!(),
            },
            (other, KTy2::Meta(meta_ty)) => match meta_ty.try_unwrap(ty_env) {
                Some(ty_cell) => ty_cell.borrow().join(other, ty_env),
                None => todo!(),
            },
            (
                KTy2::Ptr {
                    k_mut: KMut::Mut,
                    base_ty,
                },
                KTy2::Ptr {
                    k_mut: KMut::Mut,
                    base_ty: other_base_ty,
                },
            ) => {
                if base_ty == other_base_ty {
                    self.clone()
                } else {
                    KTy2::Ptr {
                        k_mut: KMut::Mut,
                        base_ty: Box::new(KTy2::Unknown),
                    }
                }
            }
            (
                KTy2::Ptr { k_mut: _, base_ty },
                KTy2::Ptr {
                    k_mut: _,
                    base_ty: other_base_ty,
                },
            ) => KTy2::Ptr {
                k_mut: KMut::Const,
                base_ty: Box::new(base_ty.join(other_base_ty, ty_env)),
            },
            (KTy2::Var(..), _)
            | (KTy2::Unit, _)
            | (KTy2::Number(..), _)
            | (KTy2::Ptr { .. }, _)
            | (KTy2::Fn { .. }, _)
            | (KTy2::ConstEnum(..), _)
            | (KTy2::StructEnum(..), _)
            | (KTy2::Struct(..), _)
            | (KTy2::App { .. }, _) => {
                if self == other {
                    self.clone()
                } else {
                    KTy2::Unknown
                }
            }
        }
    }

    pub(crate) fn align_of(&self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> Option<usize> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Unresolved { .. } | KTy2::Var { .. } | KTy2::Unknown | KTy2::Never => None,
            KTy2::Ptr { .. } | KTy2::Fn { .. } => Some(align_of::<*const ()>()),
            KTy2::Unit => Some(1),
            KTy2::Number(number_ty) => number_ty.size_of(),
            KTy2::ConstEnum(const_enum) => const_enum
                .of(&mod_outline.const_enums)
                .align_of(mod_outline),
            KTy2::StructEnum(struct_enum) => struct_enum
                .of(&mod_outline.struct_enums)
                .align_of(mod_outline),
            KTy2::Struct(k_struct) | KTy2::App { k_struct, .. } => {
                k_struct.of(&mod_outline.structs).align_of(mod_outline)
            }
            KTy2::Meta(_) => unreachable!(),
        })
    }

    pub(crate) fn size_of(&self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> Option<usize> {
        ty2_map(self, ty_env, |ty| match *ty {
            KTy2::Unresolved { .. } | KTy2::Var { .. } | KTy2::Unknown | KTy2::Never => None,
            KTy2::Ptr { .. } | KTy2::Fn { .. } => Some(size_of::<*const ()>()),
            KTy2::Unit => Some(1),
            KTy2::Number(number_ty) => number_ty.size_of(),
            KTy2::ConstEnum(const_enum) => {
                const_enum.of(&mod_outline.const_enums).size_of(mod_outline)
            }
            KTy2::StructEnum(struct_enum) => struct_enum
                .of(&mod_outline.struct_enums)
                .size_of(mod_outline),
            KTy2::Struct(k_struct) | KTy2::App { k_struct, .. } => {
                k_struct.of(&mod_outline.structs).size_of(mod_outline)
            }
            KTy2::Meta(_) => unreachable!(),
        })
    }

    fn fmt_display_with(
        &self,
        context: KTy2DisplayContext<'_>,
        f: &mut Formatter<'_>,
    ) -> fmt::Result {
        let KTy2DisplayContext {
            ty_env_opt,
            mod_outline_opt,
        } = context;

        match self {
            KTy2::Unresolved { cause } => write!(f, "{{unresolved}} ?{:?}", cause),
            KTy2::Meta(meta_ty) => match ty_env_opt.and_then(|ty_env| meta_ty.try_unwrap(ty_env)) {
                Some(ty) => ty.borrow().fmt_display_with(context, f),
                None => write!(f, "?{}", meta_ty.to_index()),
            },
            KTy2::Var(ty_var) => write!(f, "{}", ty_var.name),
            KTy2::Unknown => write!(f, "unknown"),
            KTy2::Never => write!(f, "never"),
            KTy2::Unit => write!(f, "unit"),
            KTy2::Number(number_ty) => write!(f, "{}", number_ty.as_str()),
            KTy2::Ptr { k_mut, base_ty } => {
                match k_mut {
                    KMut::Const => write!(f, "*")?,
                    KMut::Mut => write!(f, "*mut ")?,
                }
                base_ty.fmt_display_with(context, f)
            }
            KTy2::Fn {
                param_tys,
                result_ty,
            } => {
                let mut tuple = f.debug_tuple("fn");
                for param_ty in param_tys {
                    tuple.field(&DebugWith::new(param_ty, &context));
                }
                tuple.finish()?;

                if ty_env_opt.map_or(true, |ty_env| result_ty.is_unit(ty_env)) {
                    write!(f, " -> ")?;
                    result_ty.fmt_display_with(context, f)?;
                }
                Ok(())
            }
            KTy2::ConstEnum(const_enum) => match mod_outline_opt {
                Some(mod_outline) => {
                    write!(f, "enum {}", &const_enum.of(&mod_outline.const_enums).name,)
                }
                None => write!(f, "const_enum#{}", const_enum.to_index()),
            },
            KTy2::StructEnum(struct_enum) => match mod_outline_opt {
                Some(mod_outline) => write!(
                    f,
                    "enum {}",
                    &struct_enum.of(&mod_outline.struct_enums).name
                ),
                None => write!(f, "struct enum#{}", struct_enum.to_index()),
            },
            KTy2::Struct(k_struct) => match mod_outline_opt {
                Some(mod_outline) => {
                    write!(f, "struct {}", &k_struct.of(&mod_outline.structs).name)
                }
                None => write!(f, "struct#{}", k_struct.to_index()),
            },
            KTy2::App { k_struct, ty_args } => {
                KTy2::Struct(*k_struct).fmt_display_with(context, f)?;

                let mut list = f.debug_list();
                for ty_arg in ty_args.values() {
                    list.entry(&DebugWith::new(ty_arg, &context));
                }
                list.finish()
            }
        }
    }

    pub(crate) fn display(&self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> String {
        let context = KTy2DisplayContext {
            ty_env_opt: Some(ty_env),
            mod_outline_opt: Some(mod_outline),
        };
        format!("{:?}", DebugWith::new(self, &context))
    }
}

impl Default for KTy2 {
    fn default() -> Self {
        KTy2::Unresolved {
            cause: KTyCause::Default,
        }
    }
}

impl Debug for KTy2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        KModOutline::using_for_debug(|mod_outline_opt| {
            let context = KTy2DisplayContext {
                mod_outline_opt,
                ty_env_opt: None,
            };
            self.fmt_display_with(context, f)
        })
    }
}

impl<'a> DebugWithContext<KTy2DisplayContext<'a>> for KTy2 {
    fn fmt(&self, context: &KTy2DisplayContext<'a>, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_display_with(*context, f)
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct KTyParam {
    pub(crate) name: String,
    pub(crate) loc: Loc,
}

#[derive(Clone, Eq, PartialEq)]
pub(crate) struct KTyVar {
    pub(crate) name: String,
    pub(crate) loc: Loc,
}

#[derive(Clone, PartialEq)]
pub(crate) enum KTy {
    Unresolved {
        cause: KTyCause,
    },
    Var(KTyVar),
    Unknown,
    Never,
    Unit,
    Number(KNumberTy),
    Ptr {
        k_mut: KMut,
        ty: Box<KTy>,
    },
    Fn {
        ty_params: Vec<KTyParam>,
        param_tys: Vec<KTy>,
        result_ty: Box<KTy>,
    },
    Alias(KAlias),
    ConstEnum(KConstEnum),
    StructEnum(KStructEnum),
    Struct(KStruct),
    #[allow(unused)]
    StructGeneric {
        k_struct: KStruct,
        ty_params: Vec<KTyParam>,
    },
    App {
        ty: Box<KTy>,
        ty_args: Vec<KTy>,
    },
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

    /// インスタンス化して、式や項のための型を生成する。(型検査などに使う。)
    pub(crate) fn to_ty2(&self, mod_outline: &KModOutline, ty_env: &mut KTyEnv) -> KTy2 {
        do_instantiate(
            self,
            &mut TySchemeInstantiationFn::new(
                mod_outline,
                TySchemeConversionMode::Instantiate(ty_env),
            ),
        )
    }

    pub(crate) fn to_ty2_poly(&self, mod_outline: &KModOutline) -> KTy2 {
        do_instantiate(
            self,
            &mut TySchemeInstantiationFn::new(mod_outline, TySchemeConversionMode::Preserve),
        )
    }

    /// 単相の型を生成する。型変数は除去する。(コード生成などに使う。)
    pub(crate) fn erasure(&self, mod_outline: &KModOutline) -> KTy2 {
        do_instantiate(
            self,
            &mut TySchemeInstantiationFn::new(mod_outline, TySchemeConversionMode::Erasure),
        )
    }

    pub(crate) fn into_ptr(self, k_mut: KMut) -> KTy {
        KTy::Ptr {
            k_mut,
            ty: Box::new(self),
        }
    }

    pub(crate) fn is_unit(&self) -> bool {
        match self {
            KTy::Unit => true,
            _ => false,
        }
    }

    pub(crate) fn substitute(
        &self,
        mod_outline: &KModOutline,
        ty_args: &HashMap<String, KTy2>,
    ) -> KTy2 {
        do_instantiate(
            self,
            &mut TySchemeInstantiationFn::new(
                mod_outline,
                TySchemeConversionMode::Substitute(ty_args),
            ),
        )
    }

    pub(crate) fn align_of(&self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> Option<usize> {
        self.to_ty2_poly(mod_outline).align_of(ty_env, mod_outline)
    }

    pub(crate) fn size_of(&self, ty_env: &KTyEnv, mod_outline: &KModOutline) -> Option<usize> {
        self.to_ty2_poly(mod_outline).size_of(ty_env, mod_outline)
    }
}

impl Default for KTy {
    fn default() -> Self {
        KTy::Unresolved {
            cause: KTyCause::Default,
        }
    }
}

/// 型スキームを式の型に変換する際に型変数をどう扱うか
enum TySchemeConversionMode<'a> {
    /// インスタンス化: 型引数にフレッシュなメタ型を割り当てる。
    Instantiate(&'a mut KTyEnv),
    /// 型代入
    Substitute(&'a HashMap<String, KTy2>),
    /// 型変数のままにする。
    Preserve,
    /// unknown にする。
    Erasure,
}

struct TySchemeInstantiationFn<'a> {
    mod_outline: &'a KModOutline,
    /// Some なら型変数をインスタンス化する。
    /// None なら型変数は消去して unit に落とす。
    mode: TySchemeConversionMode<'a>,
    env: HashMap<String, KMetaTy>,
}

impl<'a> TySchemeInstantiationFn<'a> {
    fn new(mod_outline: &'a KModOutline, mode: TySchemeConversionMode<'a>) -> Self {
        Self {
            mod_outline,
            mode,
            env: HashMap::new(),
        }
    }
}

/// シンボルのための型スキームを式のための型に変換する処理
fn do_instantiate(ty: &KTy, context: &mut TySchemeInstantiationFn) -> KTy2 {
    match *ty {
        KTy::Unresolved { cause } => KTy2::Unresolved { cause },
        KTy::Var(ref ty_var) => match context.mode {
            TySchemeConversionMode::Instantiate(_) => {
                let meta_ty = match context.env.get(&ty_var.name).copied() {
                    Some(it) => it,
                    None => {
                        return KTy2::Unresolved {
                            cause: KTyCause::Loc(ty_var.loc),
                        };
                    }
                };
                KTy2::Meta(meta_ty)
            }
            TySchemeConversionMode::Substitute(env) => match env.get(&ty_var.name).cloned() {
                Some(it) => it,
                None => KTy2::Unresolved {
                    cause: KTyCause::Loc(ty_var.loc),
                },
            },
            TySchemeConversionMode::Preserve => KTy2::Var(ty_var.clone()),
            TySchemeConversionMode::Erasure => {
                // FIXME: この段階では型変数のままにしておく方がよい (例えば `x as T` の式の型は型変数 T のままになるはず。型を消去するのはコード生成の工程でいい)
                // DESIGN: unknown の方がいい？
                KTy2::Unit
            }
        },
        KTy::Unknown => KTy2::Unknown,
        KTy::Never => KTy2::Never,
        KTy::Unit => KTy2::Unit,
        KTy::Number(number_ty) => KTy2::Number(number_ty),
        KTy::Ptr { k_mut, ref ty } => do_instantiate(&*ty, context).into_ptr(k_mut),
        KTy::Fn {
            ref ty_params,
            ref param_tys,
            ref result_ty,
        } => {
            if let TySchemeConversionMode::Instantiate(ty_env) = &mut context.mode {
                for ty_param in ty_params {
                    let meta_ty = ty_env.alloc(KMetaTyData::new_fresh(ty_param.loc));
                    context.env.insert(ty_param.name.to_string(), meta_ty);
                }
            }

            KTy2::new_fn(
                param_tys
                    .into_iter()
                    .map(|ty| do_instantiate(&ty, context))
                    .collect::<Vec<_>>(),
                do_instantiate(&result_ty, context),
            )
        }
        KTy::Alias(alias) => match alias.of(&context.mod_outline.aliases).referent_as_ty() {
            Some(it) => it,
            None => {
                log::error!(
                    "エイリアスを解決できないか、エイリアスが型を指していません {:?}",
                    alias
                );
                return KTy2::Unresolved {
                    cause: KTyCause::Alias,
                };
            }
        },
        KTy::ConstEnum(const_enum) => KTy2::ConstEnum(const_enum),
        KTy::StructEnum(struct_enum) => KTy2::StructEnum(struct_enum),
        KTy::Struct(k_struct) => KTy2::Struct(k_struct),
        KTy::StructGeneric {
            k_struct,
            ref ty_params,
        } => {
            let mut env = take(&mut context.env);
            let ty_args = match &mut context.mode {
                TySchemeConversionMode::Instantiate(ty_env) => ty_params
                    .iter()
                    .map(|ty_param| {
                        let meta_ty = ty_env.alloc(KMetaTyData::new_fresh(ty_param.loc));
                        env.insert(ty_param.name.to_string(), meta_ty);
                        (ty_param.name.to_string(), KTy2::Meta(meta_ty))
                    })
                    .collect(),
                _ => ty_params
                    .iter()
                    .map(|ty_param| (ty_param.name.to_string(), KTy2::Unknown))
                    .collect(),
            };
            context.env = env;

            KTy2::App { k_struct, ty_args }
        }
        KTy::App {
            ref ty,
            ref ty_args,
        } => match **ty {
            KTy::Struct(k_struct) => {
                let ty_params = k_struct.of(&context.mod_outline.structs).ty_params();
                // FIXME: 型引数の個数が違ったらエラーにする
                let ty_args = ty_args
                    .iter()
                    .chain((ty_args.len()..ty_params.len()).map(|_| &KTy::Never));
                let ty_args = ty_params
                    .iter()
                    .zip(ty_args)
                    .map(|(ty_param, ty_arg)| {
                        let name = ty_param.name.to_string();
                        let ty = do_instantiate(ty_arg, context);
                        (name, ty)
                    })
                    .collect();
                KTy2::App { k_struct, ty_args }
            }
            _ => KTy2::Unresolved {
                cause: KTyCause::Default,
            },
        },
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
