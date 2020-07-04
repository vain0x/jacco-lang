use super::*;
use std::fmt::{self, Debug};

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit { location: Location },
    Int(TokenData, KTy),
    Float(TokenData),
    Char(TokenData),
    Str(TokenData),
    True(TokenData),
    False(TokenData),
    Name(KSymbol),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    Label(KLabel),
    Return(KFn),
    ExternFn(KExternFn),
    FieldTag(KFieldTag),
}

impl KTerm {
    pub(crate) fn ty<'a>(
        &self,
        outlines: &KOutlines,
        labels: &[KLabelSig],
        locals: &[KLocalData],
    ) -> KTy {
        match self {
            KTerm::Unit { .. } => KTy::Unit,
            KTerm::Int(_, ty) => ty.clone(),
            KTerm::Float(_) => KTy::F64,
            KTerm::Char(_) => KTy::C8,
            KTerm::Str(_) => KTy::C8.into_ptr(KMut::Const),
            KTerm::True(_) | KTerm::False(_) => KTy::Bool,
            KTerm::Name(symbol) => symbol.local.ty(&locals).clone(),
            KTerm::Const(k_const) => k_const.ty(&outlines.consts).clone(),
            KTerm::StaticVar(static_var) => static_var.ty(&outlines.static_vars).clone(),
            KTerm::Fn(k_fn) => k_fn.ty(&outlines.fns),
            KTerm::Label(label) => label.ty(labels),
            KTerm::Return(k_fn) => k_fn.return_ty(&outlines.fns),
            KTerm::ExternFn(extern_fn) => extern_fn.ty(&outlines.extern_fns),
            KTerm::FieldTag(field_tag) => {
                error!("don't obtain type of field tag {:?}", field_tag);
                KTy::Unresolved
            }
        }
    }

    pub(crate) fn location(&self, _outlines: &KOutlines) -> Location {
        match self {
            KTerm::Unit { location } => location.clone(),
            KTerm::Int(token, _) => token.location(),
            KTerm::Float(token) => token.location(),
            KTerm::Char(token) => token.location(),
            KTerm::Str(token) => token.location(),
            KTerm::True(token) => token.location(),
            KTerm::False(token) => token.location(),
            KTerm::Name(KSymbol { location, .. }) => location.clone(),
            KTerm::FieldTag(KFieldTag { location, .. }) => location.clone(),
            _ => {
                // FIXME: no location info
                trace!("no location for {:?}", self);
                Location::default()
            }
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
            KTerm::Const(k_const) => write!(f, "const#{}", k_const.id()),
            KTerm::StaticVar(static_var) => write!(f, "static_var#{}", static_var.id()),
            KTerm::Fn(k_fn) => {
                // FIXME: name
                write!(f, "fn#{}", k_fn.id())
            }
            KTerm::Label(label) => {
                // FIXME: name
                write!(f, "label#{}", label.id())
            }
            KTerm::Return(k_fn) => {
                // FIXME: name
                write!(f, "return#{}", k_fn.id())
            }
            KTerm::ExternFn(extern_fn) => {
                // FIXME: name
                write!(f, "extern_fn#{}", extern_fn.id())
            }
            KTerm::FieldTag(KFieldTag { name, .. }) => write!(f, "{}", name),
        }
    }
}

impl Default for KTerm {
    fn default() -> Self {
        KTerm::Unit {
            location: Location::default(),
        }
    }
}
