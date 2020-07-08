use super::TPos;
use std::{
    fmt::{self, Debug, Display, Formatter},
    iter::Sum,
    ops::{Add, AddAssign},
};

/// テキスト上の位置 (行番号, 列番号)
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    /// 行番号。0 から始まる。
    /// テキスト中の改行の個数に等しい。
    pub(crate) line: usize,

    /// 列番号。0 から始まる。UTF-16 でエンコードしたときのバイト数で表す。
    /// テキスト中の最後の改行より後にある文字列の長さに等しい。
    pub(crate) character: usize,

    pub(crate) len: u32,
}

impl Pos {
    pub const ZERO: Pos = Pos {
        line: 0,
        character: 0,
        len: 0,
    };

    pub fn new(line: usize, character: usize, len: usize) -> Pos {
        Pos {
            line,
            character,
            len: len as u32,
        }
    }

    pub fn line(self) -> usize {
        self.line
    }

    pub fn character(self) -> usize {
        self.character
    }

    pub fn len(self) -> usize {
        self.len as usize
    }
}

impl From<char> for Pos {
    fn from(c: char) -> Pos {
        if c == '\n' {
            Pos {
                line: 1,
                character: 0,
                len: 1,
            }
        } else {
            Pos {
                line: 0,
                character: c.len_utf16(),
                len: c.len_utf8() as u32,
            }
        }
    }
}

impl From<&'_ str> for Pos {
    fn from(s: &str) -> Pos {
        s.chars().map(Pos::from).sum::<Pos>()
    }
}

impl From<TPos> for Pos {
    fn from(t_pos: TPos) -> Self {
        let (line, character) = t_pos.as_pos16();
        Pos::new(line, character, t_pos.index())
    }
}

impl AddAssign for Pos {
    fn add_assign(&mut self, other: Self) {
        if other.line >= 1 {
            self.character = 0;
        }

        self.line += other.line;
        self.character += other.character;
        self.len += other.len;
    }
}

impl Add for Pos {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl Sum for Pos {
    fn sum<I: Iterator<Item = Pos>>(iter: I) -> Pos {
        iter.fold(Pos::default(), Add::add)
    }
}

impl Debug for Pos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}
