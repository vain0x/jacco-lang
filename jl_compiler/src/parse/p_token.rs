#![allow(unused)]

use crate::{
    token::{Location, TokenData, TokenKind},
    utils::TakeOut,
};

/// 構文解析フェイズから見たトークン
#[derive(Copy, Clone)]
pub(crate) struct PToken {
    id: u32,
}

impl PToken {
    pub(crate) fn new(id: usize) -> Self {
        Self { id: id as u32 }
    }

    pub(crate) fn id(self) -> usize {
        self.id as usize
    }

    pub(crate) fn kind(self, tokens: &[TokenData]) -> TokenKind {
        tokens[self.id()].kind()
    }

    pub(crate) fn text(self, tokens: &[TokenData]) -> &str {
        tokens[self.id()].text()
    }

    pub(crate) fn location(self, tokens: &[TokenData]) -> Location {
        tokens[self.id()].as_location().clone()
    }

    pub(crate) fn take_out(self, tokens: &mut [TokenData]) -> TokenData {
        (self, tokens).take_out()
    }
}

impl TakeOut<TokenData> for (PToken, &'_ mut [TokenData]) {
    fn take_out(&mut self) -> TokenData {
        let (token, tokens) = self;
        tokens[token.id()].take_out()
    }
}
