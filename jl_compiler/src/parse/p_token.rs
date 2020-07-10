#![allow(unused)]

use crate::{
    token::{HaveLocation, Location, TokenData, TokenKind},
    utils::{RawId, TakeOut, VecArena, VecArenaId},
};
use std::{
    fmt::{self, Debug, Formatter},
    num::NonZeroU32,
    ops::Index,
};

pub struct PTokenTag;

/// 構文解析フェイズから見たトークン
pub(crate) type PToken = VecArenaId<PTokenTag>;

impl PToken {
    pub(crate) fn get(self, tokens: &PTokens) -> TokenData {
        tokens[self].clone()
    }

    pub(crate) fn kind(self, tokens: &PTokens) -> TokenKind {
        tokens[self].kind()
    }

    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens[self].text()
    }

    pub(crate) fn location(self, tokens: &PTokens) -> Location {
        tokens[self].location()
    }

    pub(crate) fn take_out(self, tokens: &mut PTokens) -> TokenData {
        (self, tokens).take_out()
    }
}

impl TakeOut<TokenData> for (PToken, &'_ mut PTokens) {
    fn take_out(&mut self) -> TokenData {
        let (token, tokens) = self;
        tokens[*token].take_out()
    }
}

// impl Debug for PToken {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         write!(f, "token#{:?}", self.0)
//     }
// }

pub(crate) type PTokens = VecArena<PTokenTag, TokenData>;
