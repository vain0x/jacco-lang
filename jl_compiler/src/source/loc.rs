use super::{Doc, TRange};
use crate::parse::PLoc;
use std::fmt::Debug;

/// 位置情報
///
/// QUESTION: 中間表現が構文に依存することの是非?
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct Loc(Result<(Doc, PLoc), &'static str>);

impl Loc {
    pub(crate) fn new(doc: Doc, loc: PLoc) -> Self {
        Self(Ok((doc, loc)))
    }

    pub(crate) fn new_unknown(hint: &'static str) -> Self {
        Self(Err(hint))
    }

    pub(crate) fn inner(self) -> Result<(Doc, PLoc), &'static str> {
        self.0
    }
}

pub(crate) trait HaveLoc {
    fn loc(&self) -> Loc;
}

impl HaveLoc for (Doc, TRange) {
    fn loc(&self) -> Loc {
        let (doc, range) = *self;
        Loc::new(doc, PLoc::Range(range))
    }
}

impl<'a, T: HaveLoc> HaveLoc for &'a T {
    fn loc(&self) -> Loc {
        T::loc(*self)
    }
}

impl<'a, T: HaveLoc> HaveLoc for &'a mut T {
    fn loc(&self) -> Loc {
        T::loc(*self)
    }
}

impl HaveLoc for Loc {
    fn loc(&self) -> Loc {
        *self
    }
}
