use super::{KConst, KExternFn, KFieldTag, KFn, KLabel, KStaticVar, KSymbol, KTy};
use crate::source::Location;
use std::fmt;

/// CPS 原子項
#[derive(Clone)]
pub enum KTerm {
    Unit {
        location: Location,
    },
    Int {
        text: String,
        location: Location,
        ty: KTy,
    },
    Float {
        text: String,
        location: Location,
    },
    Char {
        text: String,
        location: Location,
    },
    Str {
        text: String,
        location: Location,
    },
    True {
        text: String,
        location: Location,
    },
    False {
        text: String,
        location: Location,
    },
    Name(KSymbol),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    Label(KLabel),
    Return(KFn),
    ExternFn(KExternFn),
    FieldTag(KFieldTag),
}

impl fmt::Debug for KTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTerm::Unit { .. } => write!(f, "()"),
            KTerm::Int { text, .. } => write!(f, "{}", text),
            KTerm::Float { text, .. } => write!(f, "{}", text),
            KTerm::Char { text, .. } => write!(f, "{}", text),
            KTerm::Str { text, .. } => write!(f, "{}", text),
            KTerm::True { text, .. } => write!(f, "{}", text),
            KTerm::False { text, .. } => write!(f, "{}", text),
            KTerm::Name(symbol) => fmt::Debug::fmt(symbol, f),
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
