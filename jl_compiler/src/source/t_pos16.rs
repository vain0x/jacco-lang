use std::{
    fmt::{self, Debug, Display, Formatter},
    iter::Sum,
    ops::{Add, AddAssign},
};

/// テキスト上の位置を UTF-16 ベースで計算したもの。
/// 主に LSP で使う。
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct TPos16 {
    /// 行番号: テキスト中の改行の個数。0 から始まる。
    row: u32,

    /// 列番号: テキスト中の最後の改行より後にある文字列の長さ。0 から始まる。
    /// 文字列の長さは UTF-16 でエンコードしたときのコードユニット数で計算する。
    column16: u32,
}

impl TPos16 {
    pub const ZERO: TPos16 = TPos16 {
        row: 0,
        column16: 0,
    };

    const LF: TPos16 = TPos16 {
        row: 1,
        column16: 0,
    };

    pub fn new(row: usize, column16: usize) -> Self {
        Self {
            row: row as u32,
            column16: column16 as u32,
        }
    }

    pub fn row(self) -> usize {
        self.row as usize
    }

    pub fn column16(self) -> usize {
        self.column16 as usize
    }
}

impl From<char> for TPos16 {
    fn from(c: char) -> Self {
        if c == '\n' {
            TPos16::LF
        } else {
            TPos16 {
                row: 0,
                column16: c.len_utf16() as u32,
            }
        }
    }
}

impl From<&'_ str> for TPos16 {
    fn from(s: &str) -> Self {
        s.chars().map(TPos16::from).sum::<TPos16>()
    }
}

impl AddAssign for TPos16 {
    fn add_assign(&mut self, other: Self) {
        if other.row >= 1 {
            self.column16 = 0;
        }

        self.row += other.row;
        self.column16 += other.column16;
    }
}

impl Add for TPos16 {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl Sum for TPos16 {
    fn sum<I: Iterator<Item = TPos16>>(iter: I) -> TPos16 {
        iter.fold(TPos16::ZERO, Add::add)
    }
}

impl Debug for TPos16 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for TPos16 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(f, "{}:{}", self.row + 1, self.column16 + 1)
    }
}
