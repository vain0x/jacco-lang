use crate::{
    token::{Location, TokenData, TokenKind},
    utils::{TakeOut, VecArena, VecArenaId},
};

pub struct PTokenTag;

/// 構文解析フェイズから見たトークン
pub(crate) type PToken = VecArenaId<PTokenTag>;

impl PToken {
    pub(crate) fn get(self, tokens: &PTokens) -> TokenData {
        tokens[self].clone()
    }

    #[allow(unused)]
    pub(crate) fn kind(self, tokens: &PTokens) -> TokenKind {
        tokens[self].kind()
    }

    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens[self].text()
    }

    pub(crate) fn location(self, tokens: &PTokens) -> Location {
        tokens[self].location()
    }

    #[allow(unused)]
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

pub(crate) type PTokens = VecArena<PTokenTag, TokenData>;
