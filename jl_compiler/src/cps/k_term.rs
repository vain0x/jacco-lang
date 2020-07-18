use super::*;
use std::fmt::{self, Debug};

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit {
        location: Location,
    },
    Int(TokenData, KTy2),
    Float(TokenData),
    Char(TokenData),
    Str(TokenData),
    True(TokenData),
    False(TokenData),
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
        outlines: &KModOutline,
        labels: &KLabelSigArena,
        locals: &KLocalArena,
    ) -> KTy2 {
        match self {
            KTerm::Unit { .. } => KTy2::UNIT,
            KTerm::Int(_, ty) => ty.clone(),
            KTerm::Float(_) => KTy2::F64,
            KTerm::Char(_) => KTy2::C8,
            KTerm::Str(_) => KTy2::C8.into_ptr(KMut::Const),
            KTerm::True(_) | KTerm::False(_) => KTy2::BOOL,
            KTerm::Name(symbol) => symbol.local.ty(&locals),
            KTerm::Alias { .. } => {
                // FIXME: 実装. プロジェクト全体の outlines を受け取る必要がある
                KTy2::Unresolved
            }
            KTerm::Const { k_const, .. } => k_const.ty(&outlines.consts).to_ty2(k_mod),
            KTerm::StaticVar { static_var, .. } => {
                static_var.ty(&outlines.static_vars).to_ty2(k_mod)
            }
            KTerm::Fn { k_fn, .. } => k_fn.ty(&outlines.fns).to_ty2(k_mod),
            KTerm::Label { label, .. } => label.ty(labels),
            KTerm::Return { k_fn, .. } => k_fn.return_ty(&outlines.fns).to_ty2(k_mod),
            KTerm::ExternFn { extern_fn, .. } => extern_fn.ty(&outlines.extern_fns).to_ty2(k_mod),
            KTerm::RecordTag { k_struct, .. } => k_struct
                .tag_ty(&outlines.structs, &outlines.enums)
                .to_ty2(k_mod),
            KTerm::FieldTag(field_tag) => {
                error!("don't obtain type of field tag {:?}", field_tag);
                KTy2::Unresolved
            }
        }
    }

    pub(crate) fn location(&self, _outlines: &KModOutline) -> Location {
        match self {
            KTerm::Unit { location } => *location,
            KTerm::Int(token, _) => token.location(),
            KTerm::Float(token) => token.location(),
            KTerm::Char(token) => token.location(),
            KTerm::Str(token) => token.location(),
            KTerm::True(token) => token.location(),
            KTerm::False(token) => token.location(),
            KTerm::Name(KSymbol { location, .. })
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
            KTerm::Int(token, _) => write!(f, "{}", token.text()),
            KTerm::Float(token) => write!(f, "{}", token.text()),
            KTerm::Char(token) => write!(f, "{}", token.text()),
            KTerm::Str(token) => write!(f, "{}", token.text()),
            KTerm::True(token) => write!(f, "{}", token.text()),
            KTerm::False(token) => write!(f, "{}", token.text()),
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
