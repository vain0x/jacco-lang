use super::Position;
use std::fmt;

/// テキスト上の範囲
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Range {
    /// 開始位置
    pub start: Position,

    /// 終了位置 (終端は範囲外)
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Range {
        Range { start, end }
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }

    #[allow(dead_code)]
    pub(crate) fn contains_loosely(self, position: Position) -> bool {
        self.start <= position && position <= self.end
    }

    pub fn unite(self, other: Range) -> Range {
        Range {
            start: self.start.min(other.start),
            end: self.start.max(other.end),
        }
    }

    pub fn ahead(self) -> Range {
        Range {
            start: self.start,
            end: self.start,
        }
    }

    pub fn behind(self) -> Range {
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
