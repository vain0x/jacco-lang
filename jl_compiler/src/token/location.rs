use super::*;
use crate::{
    source::{Doc, Loc, TRange},
    utils::TakeOut,
};
use std::fmt::{self, Debug, Formatter};

/// トークンや構文木の位置情報
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Location(Loc);

impl Location {
    pub(crate) fn new(source: TokenSource, range: TRange) -> Location {
        match source {
            TokenSource::Special(name) => Location(Loc::Unknown(name)),
            TokenSource::File(doc) => Location(Loc::Range { doc, range }),
        }
    }

    pub(crate) fn into_loc(self) -> Loc {
        self.0
    }

    pub(crate) fn range(&self) -> TRange {
        self.0.range_opt().unwrap_or(TRange::ZERO)
    }

    pub(crate) fn unite(self, other: Location) -> Location {
        Location(self.0.unite(other.into_loc()))
    }

    #[allow(unused)]
    pub(crate) fn behind(self) -> Location {
        Location(self.0.behind())
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl TakeOut for Location {
    fn take_out(&mut self) -> Self {
        *self
    }
}

pub(crate) trait HaveLocation {
    fn location(&self) -> Location;
}

impl HaveLocation for (Doc, TRange) {
    fn location(&self) -> Location {
        let (doc, range) = *self;
        Location(Loc::Range { doc, range })
    }
}

impl<'a, T: HaveLocation> HaveLocation for &'a T {
    fn location(&self) -> Location {
        T::location(*self)
    }
}

impl<'a, T: HaveLocation> HaveLocation for &'a mut T {
    fn location(&self) -> Location {
        T::location(*self)
    }
}

impl HaveLocation for Location {
    fn location(&self) -> Location {
        *self
    }
}

impl HaveLocation for TokenData {
    fn location(&self) -> Location {
        TokenData::location(self)
    }
}
