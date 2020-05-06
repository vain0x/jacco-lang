use super::*;
use std::fmt;

/// トークンや構文木の位置情報
#[derive(Clone, PartialEq, Eq, Hash)]
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
    pub(crate) fn start(&self) -> Position {
        self.range.start
    }

    #[allow(dead_code)]
    pub(crate) fn unite(self, other: &Location) -> Location {
        Location {
            range: self.range.unite(other.range),
            ..self
        }
    }
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}:{}", self.source, self.range)
    }
}

impl Default for Location {
    fn default() -> Self {
        Location::new(TokenSource::Special("<default>"), Range::default())
    }
}
