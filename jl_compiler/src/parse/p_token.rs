use crate::{
    source::TRange,
    token::TokenData,
    utils::{VecArena, VecArenaId, VecArenaSlice},
};

pub(crate) struct PTokenTag;

/// 構文解析フェイズから見たトークン
pub(crate) type PToken = VecArenaId<PTokenTag>;

pub(crate) type PTokenSlice = VecArenaSlice<PTokenTag>;

impl PToken {
    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens[self].text()
    }

    pub(crate) fn range(self, tokens: &PTokens) -> Result<TRange, &'static str> {
        Ok(tokens[self].range())
    }
}

pub(crate) type PTokens = VecArena<PTokenTag, TokenData>;
