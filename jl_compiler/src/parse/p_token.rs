use super::PLoc;
use crate::{
    source::{Loc, TRange},
    token::TokenData,
    utils::{TakeOut, VecArena, VecArenaId, VecArenaSlice},
};

pub(crate) struct PTokenTag;

/// 構文解析フェイズから見たトークン
pub(crate) type PToken = VecArenaId<PTokenTag>;

pub(crate) type PTokenSlice = VecArenaSlice<PTokenTag>;

impl PToken {
    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens[self].text()
    }

    pub(crate) fn loc(self, tokens: &PTokens) -> Loc {
        tokens[self].loc()
    }

    pub(crate) fn range(self, tokens: &PTokens) -> Result<TRange, &'static str> {
        let (_, loc) = tokens[self].loc().inner()?;
        match loc {
            PLoc::Unknown(hint) => Err(hint),
            PLoc::Range(range) => Ok(range),
            _ => unreachable!(),
        }
    }

    pub(crate) fn decompose(self, tokens: &PTokens) -> (String, Loc) {
        let token_data = self.of(tokens);
        (token_data.text().to_string(), token_data.loc())
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
