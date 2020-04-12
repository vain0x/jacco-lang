use crate::token::TokenData;

#[derive(Clone, Debug)]
pub(crate) enum KTerm {
    Int(TokenData),
    Name(TokenData),
}
