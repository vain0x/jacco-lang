use std::fmt;
use std::ops::{Add, AddAssign};

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Position {
    pub(crate) line: usize,
    pub(crate) character: usize,
}

impl Position {
    pub(crate) const ZERO: Position = Position {
        line: 0,
        character: 0,
    };

    #[allow(dead_code)]
    pub(crate) fn new(line: usize, character: usize) -> Position {
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

impl std::iter::Sum for Position {
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
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}
