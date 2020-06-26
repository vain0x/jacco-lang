use super::*;
use std::fmt;

/// テキスト上の範囲
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub(crate) struct Range {
    /// 開始位置
    pub(crate) start: Pos,

    /// 終了位置 (終端は範囲外)
    pub(crate) end: Pos,
}

impl Range {
    pub(crate) fn new(start: Pos, end: Pos) -> Range {
        Range { start, end }
    }

    #[allow(dead_code)]
    pub(crate) fn start(&self) -> Pos {
        self.start
    }

    #[allow(dead_code)]
    pub(crate) fn end(&self) -> Pos {
        self.end
    }

    #[allow(dead_code)]
    pub(crate) fn contains_loosely(self, pos: Pos) -> bool {
        self.start <= pos && pos <= self.end
    }

    pub(crate) fn unite(self, other: Range) -> Range {
        Range {
            start: self.start.min(other.start),
            end: self.start.max(other.end),
        }
    }

    pub(crate) fn ahead(self) -> Range {
        Range {
            start: self.start,
            end: self.start,
        }
    }

    pub(crate) fn behind(self) -> Range {
        Range {
            start: self.end,
            end: self.end,
        }
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
