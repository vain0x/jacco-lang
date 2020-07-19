use super::TRange;
use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display, Formatter},
    hash::{Hash, Hasher},
    iter::Sum,
    ops::{Add, AddAssign},
};

/// Text position. テキスト上の位置を複数の表現で計算したもの。
///
/// - UTF-8 コードユニット数。内部的な位置計算に使う。
/// - UTF-8 ベースの (行番号, 列番号) 表示。エラーメッセージで使う。
/// - UTF-16 ベースの (行番号, 列番号) 表示。LSP で使う。
#[derive(Copy, Clone)]
pub struct TPos {
    /// インデックス: ソースコードの先頭から、この位置の手前までの文字数。0 から始まる。
    /// 文字列の長さは UTF-8 でエンコードしたときのバイト数で計算する。
    index: u32,

    /// 行番号: テキスト中の改行の個数。0 から始まる。
    row: u32,

    /// 列番号: テキスト中の最後の改行より後にある文字列の長さ。0 から始まる。
    /// 文字列の長さは UTF-8 でエンコードしたときのバイト数で計算する。
    column8: u32,

    /// 列番号: テキスト中の最後の改行より後にある文字列の長さ。0 から始まる。
    /// 文字列の長さは UTF-16 でエンコードしたときのコードユニット数で計算する。
    column16: u32,
}

impl TPos {
    pub const ZERO: TPos = TPos {
        index: 0,
        row: 0,
        column8: 0,
        column16: 0,
    };

    pub(crate) const LF: TPos = TPos {
        index: 1,
        row: 1,
        column8: 0,
        column16: 0,
    };

    pub fn new(index: usize, row: usize, column8: usize, column16: usize) -> Self {
        Self {
            index: index as u32,
            row: row as u32,
            column8: column8 as u32,
            column16: column16 as u32,
        }
    }

    pub fn index(self) -> usize {
        self.index as usize
    }

    pub fn row(self) -> usize {
        self.row as usize
    }

    pub fn column8(self) -> usize {
        self.column8 as usize
    }

    pub fn column16(self) -> usize {
        self.column16 as usize
    }

    pub(crate) fn as_pos8(&self) -> (usize, usize) {
        (self.row(), self.column8())
    }

    pub(crate) fn as_pos16(&self) -> (usize, usize) {
        (self.row(), self.column16())
    }

    pub(crate) fn to_empty_range(&self) -> TRange {
        TRange::new(*self, *self)
    }

    /// 2つの位置の同値性に関する整合性を確認する。
    ///
    /// 同じテキストに対する位置なら、インデックスが等しいかどうかと、
    /// 残りのフィールドが等しいかどうかが一致するはず。
    pub(crate) fn assert_equality_consistency(&self, other: &Self) {
        let equal = self.index == other.index;
        debug_assert!(
            (self.as_pos8() == other.as_pos8()) == equal
                && (self.as_pos16() == other.as_pos16()) == equal,
            "{:?}",
            (self, other)
        );
    }

    /// 2つの位置の順序に関する整合性を確認する。
    ///
    /// 同じテキストに対する位置なら、インデックスの大小関係と、
    /// (行, 列) の大小関係が一致するはず。
    pub(crate) fn assert_ordering_consistency(&self, other: &Self) {
        let ordering = self.index.cmp(&other.index);
        assert!(
            self.as_pos8().cmp(&other.as_pos8()) == ordering
                && self.as_pos16().cmp(&other.as_pos16()) == ordering,
            "{:?}",
            (self, other)
        );
    }
}

impl From<char> for TPos {
    fn from(c: char) -> Self {
        if c == '\n' {
            TPos::LF
        } else {
            let column8 = c.len_utf8() as u32;
            let column16 = c.len_utf16() as u32;
            TPos {
                index: column8,
                row: 0,
                column8,
                column16,
            }
        }
    }
}

impl From<&'_ str> for TPos {
    fn from(s: &str) -> Self {
        s.chars().map(TPos::from).sum::<TPos>()
    }
}

impl AddAssign for TPos {
    fn add_assign(&mut self, other: Self) {
        if other.row >= 1 {
            self.column8 = 0;
            self.column16 = 0;
        }

        self.index += other.index;
        self.row += other.row;
        self.column8 += other.column8;
        self.column16 += other.column16;
    }
}

impl Add for TPos {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl Sum for TPos {
    fn sum<I: Iterator<Item = TPos>>(iter: I) -> TPos {
        iter.fold(TPos::ZERO, Add::add)
    }
}

impl Debug for TPos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for TPos {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // <https://www.gnu.org/prep/standards/html_node/Errors.html>
        write!(f, "{}:{}", self.row + 1, self.column8 + 1)
    }
}

impl PartialEq for TPos {
    fn eq(&self, other: &Self) -> bool {
        self.assert_equality_consistency(other);
        self.index == other.index
    }
}

impl Eq for TPos {}

impl PartialOrd for TPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.assert_ordering_consistency(other);
        self.index.partial_cmp(&other.index)
    }
}
impl Ord for TPos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.assert_ordering_consistency(other);
        self.index.cmp(&other.index)
    }
}

impl Hash for TPos {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state)
    }
}
