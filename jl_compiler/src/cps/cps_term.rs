use crate::token::{Location, TokenData};

#[derive(Clone, Debug)]
pub(crate) enum KTerm {
    Int(TokenData),
    Name { text: String, location: Location },
}
