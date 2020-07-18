use super::*;
use std::fmt::{self, Debug, Display, Formatter};

/// テキスト上の範囲
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Range {
    /// 開始位置
    pub(crate) start: Pos,

    /// 終了位置 (終端は範囲外)
    pub(crate) end: Pos,

    /// 開始位置 (UTF-8)
    pub(crate) start_index: u32,

    /// 終了位置 (UTF-8)
    pub(crate) end_index: u32,
}

impl Range {
    pub fn new(start: Pos, end: Pos) -> Range {
        Range {
            start,
            end,
            start_index: start.len() as u32,
            end_index: end.len() as u32,
        }
    }

    pub fn start(&self) -> Pos {
        self.start
    }

    pub fn end(&self) -> Pos {
        self.end
    }

    #[allow(unused)]
    pub(crate) fn start_index(&self) -> usize {
        self.start_index as usize
    }

    #[allow(unused)]
    pub(crate) fn end_index(&self) -> usize {
        self.end_index as usize
    }

    pub fn contains_loosely(self, pos: Pos) -> bool {
        self.start <= pos && pos <= self.end
    }

    #[allow(unused)]
    pub(crate) fn unite(self, other: Range) -> Range {
        Range {
            start: self.start.min(other.start),
            end: self.start.max(other.end),
            start_index: self.start_index.min(other.start_index),
            end_index: self.end_index.max(other.end_index),
        }
    }

    #[allow(unused)]
    pub(crate) fn ahead(self) -> Range {
        Range {
            start: self.start,
            end: self.start,
            start_index: self.start_index,
            end_index: self.start_index,
        }
    }

    #[allow(unused)]
    pub(crate) fn behind(self) -> Range {
        Range {
            start: self.end,
            end: self.end,
            start_index: self.end_index,
            end_index: self.end_index,
        }
    }
}

impl From<TRange> for Range {
    fn from(t_range: TRange) -> Self {
        Range::new(t_range.start().into(), t_range.end().into())
    }
}

impl From<Range> for TRange {
    fn from(range: Range) -> Self {
        TRange::new(range.start.into(), range.end.into())
    }
}

impl Debug for Range {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(
            f,
            "{}.{}-{}.{}",
            self.start.line + 1,
            self.start.character + 1,
            self.end.line + 1,
            self.end.character + 1
        )
    }
}
