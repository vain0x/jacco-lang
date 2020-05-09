//! Jacco 言語の構文木の定義

use super::*;

pub(crate) type PNameId = usize;

#[derive(Clone, Debug)]
pub(crate) struct PName {
    pub(crate) name_id: PNameId,
    pub(crate) text: String,
    pub(crate) location: Location,
}

impl PName {
    pub(crate) fn as_str(&self) -> &str {
        self.text.as_str()
    }
}

#[derive(Clone, Debug)]
pub(crate) enum PTy {
    Name(PName),
    Never {
        bang: TokenData,
    },
    Unit {
        left: TokenData,
        right_opt: Option<TokenData>,
    },
    Ptr {
        star: TokenData,
        ty_opt: Option<Box<PTy>>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct PParam {
    pub(crate) name: PName,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) comma_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PParamList {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
    pub(crate) params: Vec<PParam>,
}

/// 結果型注釈 (`-> T`)
#[derive(Clone, Debug)]
pub(crate) struct PResult {
    pub(crate) arrow: TokenData,
    pub(crate) ty_opt: Option<PTy>,
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
    Int(TokenData),
    Str(TokenData),
    Name(PName),
    Call {
        callee: Box<PExpr>,
        arg_list: PArgList,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<PExpr>,
        right_opt: Option<Box<PExpr>>,
        location: Location,
    },
    Block(PBlock),
    Break {
        keyword: TokenData,
    },
    Continue {
        keyword: TokenData,
    },
    If {
        keyword: TokenData,
        cond_opt: Option<Box<PExpr>>,
        body_opt: Option<PBlock>,
        else_opt: Option<TokenData>,
        alt_opt: Option<Box<PExpr>>,
    },
    While {
        keyword: TokenData,
        cond_opt: Option<Box<PExpr>>,
        body_opt: Option<PBlock>,
    },
    Loop {
        keyword: TokenData,
        body_opt: Option<PBlock>,
    },
}

#[derive(Clone, Debug)]
pub(crate) enum PDecl {
    Expr {
        expr: PExpr,
        semi_opt: Option<TokenData>,
    },
    Let {
        keyword: TokenData,
        name_opt: Option<PName>,
        init_opt: Option<PExpr>,
    },
    Fn {
        keyword: TokenData,
        name_opt: Option<PName>,
        param_list_opt: Option<PParamList>,
        result_opt: Option<PResult>,
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
