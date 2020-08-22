use std::fmt::{self, Debug, Formatter};

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum KNumber {
    INN(i64),
    UNN(u64),
    FNN(f64),
    CNN(u32),
}

impl Debug for KNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KNumber::INN(value) => Debug::fmt(value, f),
            KNumber::UNN(value) => Debug::fmt(value, f),
            KNumber::FNN(value) => Debug::fmt(value, f),
            KNumber::CNN(value) => Debug::fmt(value, f),
        }
    }
}
