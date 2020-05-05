use super::*;
use std::fmt;

/// CPS 原子項
#[derive(Clone)]
pub(crate) enum KTerm {
    Unit { location: Location },
    Int(TokenData),
    Name(KSymbol),
}

impl fmt::Debug for KTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTerm::Unit { .. } => write!(f, "()"),
            KTerm::Int(token) => write!(f, "{}", token.text()),
            KTerm::Name(symbol) => fmt::Debug::fmt(symbol, f),
        }
    }
}
