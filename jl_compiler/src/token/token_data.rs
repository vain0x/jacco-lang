use super::*;
use crate::{source::Loc, utils::TakeOut};
use std::fmt::{self, Debug, Formatter};

/// 字句データ
#[derive(Clone)]
pub(crate) struct TokenData {
    kind: TokenKind,
    text: String,
    location: Loc,
}

impl TokenData {
    pub(crate) fn new(kind: TokenKind, text: String, location: Loc) -> Self {
        TokenData {
            kind,
            text,
            location,
        }
    }

    pub(crate) fn kind(&self) -> TokenKind {
        self.kind
    }

    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    pub(crate) fn location(&self) -> Loc {
        self.location
    }
}

impl TakeOut for TokenData {
    fn take_out(&mut self) -> TokenData {
        TokenData::new(
            self.kind.take_out(),
            self.text.take_out(),
            self.location.take_out(),
        )
    }
}

impl Debug for TokenData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TokenKind::Comment
            | TokenKind::Space
            | TokenKind::Number
            | TokenKind::Char
            | TokenKind::Str
            | TokenKind::Ident => write!(f, "{:?}", self.text()),
            _ => write!(f, "{:?}", self.kind()),
        }
    }
}
