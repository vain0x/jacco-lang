use crate::token::TokenData;

/// Visibility. 可視性
#[derive(Copy, Clone, Debug)]
pub(crate) enum Vis {
    Pub,
}

pub(crate) type PVis = (Vis, TokenData);
