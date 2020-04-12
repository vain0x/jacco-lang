use super::*;
use std::fmt;

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum TokenSource {
    Special(&'static str),
    File(SourceFile),
}

impl fmt::Debug for TokenSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenSource::Special(name) => write!(f, "{}", name),
            TokenSource::File(file) => write!(f, "{:?}", file),
        }
    }
}
