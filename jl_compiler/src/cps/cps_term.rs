use super::*;

#[derive(Clone, Debug)]
pub(crate) enum KTerm {
    Int(TokenData),
    Name(KSymbol),
}
