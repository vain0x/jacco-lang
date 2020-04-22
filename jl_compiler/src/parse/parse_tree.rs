use super::*;

#[derive(Clone, Debug)]
pub(crate) enum PTerm {
    Int(TokenData),
    Str(TokenData),
    Name(TokenData),
    BinaryOp {
        op: BinaryOp,
        left: Box<PTerm>,
        right: Box<PTerm>,
        location: Location,
    },
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
    Let {
        keyword: TokenData,
        name_opt: Option<TokenData>,
        init_opt: Option<PTerm>,
    },
    Fn {
        keyword: TokenData,
        block_opt: Option<PBlock>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) body: Vec<PStmt>,
    pub(crate) eof: TokenData,
}
