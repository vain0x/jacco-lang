use super::*;
use crate::utils::TakeOut;
use std::fmt::{self, Debug, Formatter};

/// トークンや構文木の位置情報
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Location {
    pub(crate) source: TokenSource,
    pub(crate) range: Range,
}

impl Location {
    pub(crate) fn new(source: TokenSource, range: Range) -> Location {
        Location { source, range }
    }

    #[allow(dead_code)]
    pub(crate) fn range(&self) -> Range {
        self.range
    }

    #[allow(dead_code)]
    pub(crate) fn start(&self) -> Pos {
        self.range.start
    }

    #[allow(dead_code)]
    pub(crate) fn unite(self, other: Location) -> Location {
        Location {
            range: self.range.unite(other.range),
            ..self
        }
    }

    #[allow(unused)]
    pub(crate) fn ahead(self) -> Location {
        Location {
            range: self.range.ahead(),
            ..self
        }
    }

    pub(crate) fn behind(self) -> Location {
        Location {
            range: self.range.behind(),
            ..self
        }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}:{}", self.source, self.range)
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
