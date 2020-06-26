use super::*;
use std::fmt::{self, Debug, Formatter};

/// 字句のもとになるもの
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum TokenSource {
    /// コンパイラ側で生成された字句などの出処となるもの。
    Special(&'static str),
    File(SourceFile),
}

impl Debug for TokenSource {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenSource::Special(name) => write!(f, "{}", name),
            TokenSource::File(file) => write!(f, "{:?}", file),
        }
    }
}
