use super::{Doc, TRange};
use crate::parse::PToken;
use std::{
    fmt::{self, Debug, Formatter},
    path::Path,
};

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum LocPart {
    Range,
    Ahead,
    Behind,
}

impl LocPart {
    pub(crate) fn apply(self, range: TRange) -> TRange {
        match self {
            LocPart::Range => range,
            LocPart::Ahead => range.ahead(),
            LocPart::Behind => range.behind(),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct Loc {
    doc: Doc,
    token: PToken,
    part: LocPart,
}

impl Loc {
    #[allow(unused)]
    pub(crate) fn doc(&self) -> Doc {
        self.doc
    }

    #[allow(unused)]
    pub(crate) fn token(&self) -> PToken {
        self.token
    }

    #[allow(unused)]
    pub(crate) fn part(&self) -> LocPart {
        self.part
    }

}

impl Debug for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.doc, f)?;

        // FIXME: トークンの位置情報
        write!(f, " ")?;
        Debug::fmt(&self.token, f)?;

        match self.part {
            LocPart::Range => {}
            LocPart::Ahead => write!(f, ":ahead")?,
            LocPart::Behind => write!(f, ":behind")?,
        }
        Ok(())
    }
}

impl Loc {
    #[allow(unused)]
    pub(crate) fn ahead(self) -> Self {
        Self {
            part: LocPart::Ahead,
            ..self
        }
    }

    #[allow(unused)]
    pub(crate) fn behind(self) -> Self {
        Self {
            part: LocPart::Behind,
            ..self
        }
    }
}

pub(crate) trait LocResolver {
    fn doc_path(&self, doc: Doc) -> Option<&Path>;

    fn token_range(&self, doc: Doc, token: PToken) -> TRange;
}
