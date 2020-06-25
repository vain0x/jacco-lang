use super::*;
use std::fmt;

/// 字句のもとになるもの
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TokenSource {
    /// コンパイラ側で生成された字句などの出処となるもの。
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
