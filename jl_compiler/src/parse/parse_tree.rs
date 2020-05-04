use super::*;

pub(crate) type PNameId = usize;

#[derive(Clone, Debug)]
pub(crate) struct PName {
    pub(crate) name_id: PNameId,
    pub(crate) text: String,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) enum PTerm {
    Int(TokenData),
    Str(TokenData),
    Name(PName),
    Call {
        callee: Box<PTerm>,
        arg_list: PArgList,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<PTerm>,
        right_opt: Option<Box<PTerm>>,
        location: Location,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct PParam {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PName>,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PParamList {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
    pub(crate) params: Vec<PParam>,
}

#[derive(Clone, Debug)]
pub(crate) struct PResult {
    pub(crate) arrow: TokenData,
    pub(crate) ty_opt: Option<PName>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArg {
    pub(crate) expr: PExpr,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PArgList {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
    pub(crate) args: Vec<PArg>,
}

#[derive(Clone, Debug)]
pub(crate) struct PBlock {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
    pub(crate) decls: Vec<PDecl>,
    pub(crate) last_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) enum PExpr {
    Term {
        term: PTerm,
        semi_opt: Option<TokenData>,
    },
    Block(PBlock),
    Break {
        keyword: TokenData,
        semi_opt: Option<TokenData>,
    },
    Continue {
        keyword: TokenData,
        semi_opt: Option<TokenData>,
    },
    If {
        keyword: TokenData,
        cond_opt: Option<PTerm>,
        body_opt: Option<PBlock>,
        else_opt: Option<TokenData>,
        alt_opt: Option<Box<PExpr>>,
    },
    While {
        keyword: TokenData,
        cond_opt: Option<PTerm>,
        body_opt: Option<PBlock>,
    },
    Loop {
        keyword: TokenData,
        body_opt: Option<PBlock>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum PDecl {
    Expr(PExpr),
    Let {
        keyword: TokenData,
        name_opt: Option<PName>,
        init_opt: Option<PExpr>,
    },
    Fn {
        keyword: TokenData,
        block_opt: Option<PBlock>,
    },
    ExternFn {
        extern_keyword: TokenData,
        fn_keyword: TokenData,
        name_opt: Option<PName>,
        param_list_opt: Option<PParamList>,
        result_opt: Option<PResult>,
        semi_opt: Option<TokenData>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: TokenData,
}
