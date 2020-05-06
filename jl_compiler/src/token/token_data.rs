use super::*;
use std::fmt;

/// 字句データ
#[derive(Clone)]
pub(crate) struct TokenData {
    kind: TokenKind,
    text: String,
    location: Location,
}

impl TokenData {
    pub(crate) fn new(kind: TokenKind, text: String, location: Location) -> Self {
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

    pub(crate) fn into_text(self) -> String {
        self.text
    }

    #[allow(dead_code)]
    pub(crate) fn location(&self) -> &Location {
        &self.location
    }

    pub(crate) fn into_location(self) -> Location {
        self.location
    }

    pub(crate) fn decompose(self) -> (TokenKind, String, Location) {
        (self.kind, self.text, self.location)
    }
}

impl fmt::Debug for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TokenKind::Comment
            | TokenKind::Int
            | TokenKind::Space
            | TokenKind::Float
            | TokenKind::Char
            | TokenKind::Str
            | TokenKind::Ident => write!(f, "{:?}", self.text()),
            _ => write!(f, "{:?}", self.kind()),
        }
    }
}

impl Default for TokenData {
    fn default() -> Self {
        TokenData {
            kind: TokenKind::Other,
            text: String::default(),
            location: Location::default(),
        }
    }
}
