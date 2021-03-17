use super::Doc;
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
