use super::*;
use crate::{
    source::{Doc, Loc, TRange},
    utils::TakeOut,
};
use std::fmt::{self, Debug, Formatter};

/// トークンや構文木の位置情報
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Location {
    Default(&'static str),
    Loc(Loc),
}

impl Location {
    pub(crate) fn new(source: TokenSource, range: TRange) -> Location {
        match source {
            TokenSource::Special(name) => Location::Default(name),
            TokenSource::File(doc) => Location::Loc(Loc::Range { doc, range }),
        }
    }

    pub(crate) fn range(&self) -> TRange {
        match self {
            Location::Loc(Loc::Range { range, .. }) => *range,
            _ => TRange::ZERO,
        }
    }

    #[allow(dead_code)]
    pub(crate) fn unite(self, other: Location) -> Location {
        match (self, other) {
            (Location::Default(_), Location::Default(_)) => self,
            (Location::Loc(left), Location::Loc(right)) => Location::Loc(left.unite(right)),
            (Location::Loc(loc), _) | (_, Location::Loc(loc)) => Location::Loc(loc),
        }
    }

    #[allow(unused)]
    pub(crate) fn behind(self) -> Location {
        match self {
            Location::Default(_) => self,
            Location::Loc(loc) => Location::Loc(loc.behind()),
        }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Location::Default(name) => write!(f, "{}", name),
            Location::Loc(loc) => Debug::fmt(loc, f),
        }
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
        Location::Loc(Loc::Range { doc, range })
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
