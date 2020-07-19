use super::*;
use crate::token::Location;
use std::fmt::{self, Debug};

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit {
        location: Location,
    },
    Int {
        text: String,
        ty: KTy2,
        location: Location,
    },
    Float {
        text: String,
        ty: KTy2,
        location: Location,
    },
    Char {
        text: String,
        ty: KTy2,
        location: Location,
    },
    Str {
        text: String,
        location: Location,
    },
    True {
        location: Location,
    },
    False {
        location: Location,
    },
    Name(KSymbol),
    Alias {
        alias: KAlias,
        location: Location,
    },
    Const {
        k_const: KConst,
        location: Location,
    },
    StaticVar {
        static_var: KStaticVar,
        location: Location,
    },
    Fn {
        k_fn: KFn,
        location: Location,
    },
    Label {
        label: KLabel,
        location: Location,
    },
    Return {
        k_fn: KFn,
        location: Location,
    },
    ExternFn {
        extern_fn: KExternFn,
        location: Location,
    },
    RecordTag {
        k_struct: KStruct,
        location: Location,
    },
    FieldTag(KFieldTag),
}

impl KTerm {
    pub(crate) fn ty<'a>(
        &self,
        k_mod: KMod,
        mod_outline: &KModOutline,
        labels: &KLabelSigArena,
        locals: &KLocalArena,
    ) -> KTy2 {
        match self {
            KTerm::Unit { .. } => KTy2::Unit,
            KTerm::Int { ty, .. } => ty.clone(),
            KTerm::Float { ty, .. } => ty.clone(),
            KTerm::Char { ty, .. } => ty.clone(),
            KTerm::Str { .. } => KTy2::C8.into_ptr(KMut::Const),
            KTerm::True { .. } | KTerm::False { .. } => KTy2::BOOL,
            KTerm::Name(symbol) => symbol.local.ty(&locals),
            KTerm::Alias { .. } => {
                // FIXME: 実装. mod_outlines を受け取る必要がある
                KTy2::Unresolved
            }
            KTerm::Const { k_const, .. } => k_const.ty(&mod_outline.consts).to_ty2(k_mod),
            KTerm::StaticVar { static_var, .. } => {
                static_var.ty(&mod_outline.static_vars).to_ty2(k_mod)
            }
            KTerm::Fn { k_fn, .. } => k_fn.ty(&mod_outline.fns).to_ty2(k_mod),
            KTerm::Label { label, .. } => label.ty(labels),
            KTerm::Return { k_fn, .. } => k_fn.return_ty(&mod_outline.fns).to_ty2(k_mod),
            KTerm::ExternFn { extern_fn, .. } => {
                extern_fn.ty(&mod_outline.extern_fns).to_ty2(k_mod)
            }
            KTerm::RecordTag { k_struct, .. } => k_struct
                .tag_ty(&mod_outline.structs, &mod_outline.enum_reprs)
                .to_ty2(k_mod),
            KTerm::FieldTag(field_tag) => {
                error!("don't obtain type of field tag {:?}", field_tag);
                KTy2::Unresolved
            }
        }
    }

    pub(crate) fn location(&self, _outlines: &KModOutline) -> Location {
        match self {
            KTerm::Unit { location }
            | KTerm::Int { location, .. }
            | KTerm::Float { location, .. }
            | KTerm::Char { location, .. }
            | KTerm::Str { location, .. }
            | KTerm::True { location }
            | KTerm::False { location }
            | KTerm::Name(KSymbol { location, .. })
            | KTerm::Alias { location, .. }
            | KTerm::Const { location, .. }
            | KTerm::StaticVar { location, .. }
            | KTerm::Fn { location, .. }
            | KTerm::Label { location, .. }
            | KTerm::Return { location, .. }
            | KTerm::ExternFn { location, .. }
            | KTerm::RecordTag { location, .. }
            | KTerm::FieldTag(KFieldTag { location, .. }) => *location,
        }
    }
}

impl Debug for KTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTerm::Unit { .. } => write!(f, "()"),
            KTerm::Int { text, .. } => write!(f, "{}", text),
            KTerm::Float { text, .. } => write!(f, "{}", text),
            KTerm::Char { text, .. } => write!(f, "{}", text),
            KTerm::Str { text, .. } => write!(f, "{}", text),
            KTerm::True { .. } => write!(f, "true"),
            KTerm::False { .. } => write!(f, "false"),
            KTerm::Name(symbol) => Debug::fmt(symbol, f),
            KTerm::Alias { alias, .. } => write!(f, "alias#{}", alias.to_index()),
            KTerm::Const { k_const, .. } => write!(f, "const#{}", k_const.to_index()),
            KTerm::StaticVar { static_var, .. } => {
                write!(f, "static_var#{}", static_var.to_index())
            }
            KTerm::Fn { k_fn, .. } => {
                // FIXME: name
                write!(f, "fn#{}", k_fn.to_index())
            }
            KTerm::Label { label, .. } => {
                // FIXME: name
                write!(f, "label#{}", label.to_index())
            }
            KTerm::Return { k_fn, .. } => {
                // FIXME: name
                write!(f, "return#{}", k_fn.to_index())
            }
            KTerm::ExternFn { extern_fn, .. } => {
                // FIXME: name
                write!(f, "extern_fn#{}", extern_fn.to_index())
            }
            KTerm::RecordTag { k_struct, .. } => {
                // FIXME: name
                write!(f, "struct_tag#{}", k_struct.to_index())
            }
            KTerm::FieldTag(KFieldTag { name, .. }) => write!(f, "{}", name),
        }
    }
}
