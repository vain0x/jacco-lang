use crate::{
    token::{Location, TokenData},
    utils::{TakeOut, VecArena, VecArenaId},
};

pub struct PTokenTag;

/// 構文解析フェイズから見たトークン
pub(crate) type PToken = VecArenaId<PTokenTag>;

impl PToken {
    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens[self].text()
    }

    pub(crate) fn location(self, tokens: &PTokens) -> Location {
        tokens[self].location()
    }

    pub(crate) fn decompose(self, tokens: &PTokens) -> (String, Location) {
        let token_data = self.of(tokens);
        (token_data.text().to_string(), token_data.location())
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
