#![allow(unused)]

use crate::{
    token::{HaveLocation, Location, TokenData, TokenKind},
    utils::{RawId, TakeOut},
};
use std::{
    fmt::{self, Debug, Formatter},
    num::NonZeroU32,
    ops::Index,
};

/// 構文解析フェイズから見たトークン
#[derive(Copy, Clone)]
pub(crate) struct PToken {
    raw_id: RawId,
}

impl PToken {
    pub(crate) fn new(id: usize) -> Self {
        Self { raw_id: id.into() }
    }

    pub(crate) fn id(self) -> usize {
        self.raw_id.into()
    }

    pub(crate) fn get(&self, tokens: &PTokens) -> TokenData {
        tokens[self.id()].clone()
    }

    pub(crate) fn kind(self, tokens: &PTokens) -> TokenKind {
        tokens.tokens[self.id()].kind()
    }

    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens.tokens[self.id()].text()
    }

    pub(crate) fn location(self, tokens: &PTokens) -> Location {
        tokens.tokens[self.id()].location()
    }

    pub(crate) fn take_out(self, tokens: &mut PTokens) -> TokenData {
        (self, tokens).take_out()
    }
}

impl TakeOut<TokenData> for (PToken, &'_ mut PTokens) {
    fn take_out(&mut self) -> TokenData {
        let (token, tokens) = self;
        tokens.tokens[token.id()].take_out()
    }
}

impl Debug for PToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "token#{}", self.id())
    }
}

// FIXME: remove Clone
#[derive(Clone, Debug, Default)]
pub(crate) struct PTokens {
    tokens: Vec<TokenData>,
}

impl PTokens {
    pub(crate) fn new(tokens: Vec<TokenData>) -> Self {
        assert_eq!(tokens.last().map(TokenData::kind), Some(TokenKind::Eof));

        PTokens { tokens }
    }

    pub(crate) fn len(&self) -> usize {
        self.tokens.len()
    }
}

impl Index<usize> for PTokens {
    type Output = TokenData;

    fn index(&self, index: usize) -> &TokenData {
        &self.tokens[index]
    }
}
