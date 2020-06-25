use std::fmt;
use std::iter::Sum;
use std::ops::{Add, AddAssign};

/// テキスト上の位置 (行番号, 列番号)
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    /// 行番号。0 から始まる。
    /// テキスト中の改行の個数に等しい。
    pub line: usize,

    /// 列番号。0 から始まる。UTF-16 でエンコードしたときのバイト数で表す。
    /// テキスト中の最後の改行より後にある文字列の長さに等しい。
    pub character: usize,
}

impl Position {
    pub const ZERO: Position = Position {
        line: 0,
        character: 0,
    };

    pub fn new(line: usize, character: usize) -> Position {
        Position { line, character }
    }
}

impl From<char> for Position {
    fn from(c: char) -> Position {
        if c == '\n' {
            Position {
                line: 1,
                character: 0,
            }
        } else {
            Position {
                line: 0,
                character: c.len_utf16(),
            }
        }
    }
}

impl From<&'_ str> for Position {
    fn from(s: &str) -> Position {
        s.chars().map(Position::from).sum::<Position>()
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, other: Self) {
        if other.line >= 1 {
            self.character = 0;
        }

        self.line += other.line;
        self.character += other.character;
    }
}

impl Add for Position {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl Sum for Position {
    fn sum<I: Iterator<Item = Position>>(iter: I) -> Position {
        iter.fold(Position::default(), Add::add)
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}
