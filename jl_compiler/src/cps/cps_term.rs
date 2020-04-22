use super::*;
use std::fmt;

#[derive(Clone)]
pub(crate) enum KTerm {
    Int(TokenData),
    Name(KSymbol),
}

impl fmt::Debug for KTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KTerm::Int(token) => write!(f, "{}", token.text()),
            KTerm::Name(symbol) => fmt::Debug::fmt(symbol, f),
        }
    }
}
