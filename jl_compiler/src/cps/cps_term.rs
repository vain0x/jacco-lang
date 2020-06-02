use super::*;
use std::fmt;

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit { location: Location },
    Int(TokenData),
    Name(KSymbol),
    Fn(KFn),
    FieldTag(KFieldTag),
}

impl fmt::Debug for KTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTerm::Unit { .. } => write!(f, "()"),
            KTerm::Int(token) => write!(f, "{}", token.text()),
            KTerm::Name(symbol) => fmt::Debug::fmt(symbol, f),
            KTerm::Fn(k_fn) => {
                // FIXME: name
                write!(f, "fn#{}", k_fn.id())
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
