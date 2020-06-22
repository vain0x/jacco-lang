use super::*;
use std::fmt;

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit { location: Location },
    Int(TokenData, KTy),
    Char(TokenData),
    Str(TokenData),
    True(TokenData),
    False(TokenData),
    Name(KSymbol),
    Const(KConst),
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
            KTerm::Int(token, _) => write!(f, "{}", token.text()),
            KTerm::Char(token) => write!(f, "{}", token.text()),
            KTerm::Str(token) => write!(f, "{}", token.text()),
            KTerm::True(token) => write!(f, "{}", token.text()),
            KTerm::False(token) => write!(f, "{}", token.text()),
            KTerm::Name(symbol) => fmt::Debug::fmt(symbol, f),
            KTerm::Const(k_const) => write!(f, "const#{}", k_const.id()),
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
