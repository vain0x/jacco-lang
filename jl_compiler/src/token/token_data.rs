use super::*;
use crate::{source::TRange, utils::TakeOut};
use std::fmt::{self, Debug, Formatter};

/// 字句データ
#[derive(Clone)]
pub(crate) struct TokenData {
    kind: TokenKind,
    text: String,
    range: TRange,
}

impl TokenData {
    pub(crate) fn new(kind: TokenKind, text: String, range: TRange) -> Self {
        TokenData { kind, text, range }
    }

    pub(crate) fn kind(&self) -> TokenKind {
        self.kind
    }

    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    pub(crate) fn range(&self) -> TRange {
        self.range
    }
}

impl TakeOut for TokenData {
    fn take_out(&mut self) -> TokenData {
        TokenData::new(self.kind.take_out(), self.text.take_out(), self.range)
    }
}

impl Debug for TokenData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TokenKind::Space
            | TokenKind::Comment
            | TokenKind::Other
            | TokenKind::Number
            | TokenKind::Char
            | TokenKind::Str
            | TokenKind::Ident => write!(f, "{:?}", self.text()),
            _ => write!(f, "{:?}", self.kind()),
        }
    }
}
