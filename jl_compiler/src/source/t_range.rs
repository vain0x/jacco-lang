use super::{Pos, TPos};
use std::fmt::{self, Debug, Display, Formatter};

/// テキスト上の範囲
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct TRange {
    /// 開始位置
    start: TPos,

    /// 終了位置 (終端は範囲外)
    end: TPos,
}

impl TRange {
    #[allow(unused)]
    pub(crate) const ZERO: TRange = TRange {
        start: TPos::ZERO,
        end: TPos::ZERO,
    };

    pub fn new(start: TPos, end: TPos) -> Self {
        assert!(start <= end);

        Self { start, end }
    }

    pub fn start(&self) -> TPos {
        self.start
    }

    pub fn end(&self) -> TPos {
        self.end
    }

    pub fn contains_loosely(self, pos: TPos) -> bool {
        self.start <= pos && pos <= self.end
    }

    pub fn contains_loosely_pos(self, pos: Pos) -> bool {
        Pos::from(self.start) <= pos && pos <= Pos::from(self.end)
    }

    pub(crate) fn unite(self, other: &Self) -> TRange {
        TRange {
            start: self.start.min(other.start),
            end: self.start.max(other.end),
        }
    }

    pub(crate) fn behind(self) -> TRange {
        self.end.to_empty_range()
    }
}

impl Debug for TRange {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for TRange {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(
            f,
            "{}.{}-{}.{}",
            self.start.row() + 1,
            self.start.column8() + 1,
            self.end.row() + 1,
            self.end.column8() + 1
        )
    }
}
