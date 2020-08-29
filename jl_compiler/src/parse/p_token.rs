use crate::{
    source::TRange,
    token::TokenData,
    utils::{VecArena, VecArenaId, VecArenaSlice},
};

pub(crate) struct PTokenTag;

/// 構文解析フェイズから見たトークン
pub(crate) type PToken = VecArenaId<PTokenTag>;

#[allow(unused)]
pub(crate) type PTokenSlice = VecArenaSlice<PTokenTag>;

impl PToken {
    pub(crate) fn text(self, tokens: &PTokens) -> &str {
        tokens[self].text()
    }

    pub(crate) fn range(self, tokens: &PTokens) -> TRange {
        tokens[self].range()
    }
}

pub(crate) type PTokens = VecArena<PTokenTag, TokenData>;
