#![allow(unused)]

use std::fmt::{self, Debug, Formatter};

/// テキストドキュメントの ID
///
/// テキストドキュメント: ファイルやエディターのタブなど、ソースコードの出処となるもの
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct Doc {
    id: u32,
}

impl Doc {
    pub(crate) fn new(id: usize) -> Self {
        Self { id: id as u32 }
    }

    pub(crate) fn id(self) -> usize {
        self.id as usize
    }
}

impl Debug for Doc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "doc#{}", self.id)
    }
}
