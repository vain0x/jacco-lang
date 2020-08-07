use crate::source::Doc;
use std::fmt::{self, Debug, Display, Formatter};

/// 字句のもとになるもの
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TokenSource {
    #[allow(unused)]
    /// コンパイラ側で生成された字句などの出処となるもの。
    Special(&'static str),
    File(Doc),
}

impl Debug for TokenSource {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenSource::Special(name) => Display::fmt(name, f),
            TokenSource::File(doc) => Debug::fmt(doc, f),
        }
    }
}
