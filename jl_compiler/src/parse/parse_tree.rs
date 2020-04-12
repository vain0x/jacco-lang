use super::*;

#[derive(Clone, Debug)]
pub(crate) enum PTerm {
    Int(TokenData),
    Str(TokenData),
    Name(TokenData),
}

#[derive(Clone, Debug)]
pub(crate) struct PBlock {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
    pub(crate) body: Vec<PStmt>,
    pub(crate) last_opt: Option<PTerm>,
}

#[derive(Clone, Debug)]
pub(crate) enum PStmt {
    Expr {
        term: PTerm,
        semi_opt: Option<TokenData>,
    },
    Fn {
        keyword: Location,
        block_opt: Option<PBlock>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) body: Option<PStmt>,
    pub(crate) eof: TokenData,
}
