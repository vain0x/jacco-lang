//! Jacco 言語の構文木の定義

use super::*;

pub(crate) type PNameId = usize;

#[derive(Clone, Debug)]
pub(crate) struct PName {
    pub(crate) name_id: PNameId,
    pub(crate) token: TokenData,
}

impl PName {
    pub(crate) fn text(&self) -> &str {
        self.token.text()
    }

    pub(crate) fn location(&self) -> &Location {
        self.token.location()
    }

    pub(crate) fn decompose(self) -> (String, Location) {
        let (_, text, location) = self.token.decompose();
        (text, location)
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

impl PArgList {
    pub(crate) fn is_tuple(&self) -> bool {
        match self.args.as_slice() {
            []
            | [PArg {
                comma_opt: Some(_), ..
            }]
            | [_, _, ..] => true,
            _ => false,
        }
    }
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
    Tuple(PArgList),
    Call {
        callee: Box<PExpr>,
        arg_list: PArgList,
    },
    UnaryOp {
        op: PUnaryOp,
        arg_opt: Option<Box<PExpr>>,
        location: Location,
    },
    BinaryOp {
        op: PBinaryOp,
        left: Box<PExpr>,
        right_opt: Option<Box<PExpr>>,
        location: Location,
    },
    Block(PBlock),
    Break {
        keyword: TokenData,
        arg_opt: Option<Box<PExpr>>,
    },
    Continue {
        keyword: TokenData,
    },
    Return {
        keyword: TokenData,
        arg_opt: Option<Box<PExpr>>,
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

impl Default for PExpr {
    fn default() -> Self {
        PExpr::Str(TokenData::default())
    }
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
        colon_opt: Option<TokenData>,
        ty_opt: Option<PTy>,
        equal_opt: Option<TokenData>,
        init_opt: Option<PExpr>,
        semi_opt: Option<TokenData>,
    },
    Fn {
        keyword: TokenData,
        name_opt: Option<PName>,
        param_list_opt: Option<PParamList>,
        arrow_opt: Option<TokenData>,
        result_opt: Option<PTy>,
        block_opt: Option<PBlock>,
    },
    ExternFn {
        extern_keyword: TokenData,
        fn_keyword: TokenData,
        name_opt: Option<PName>,
        param_list_opt: Option<PParamList>,
        arrow_opt: Option<TokenData>,
        result_opt: Option<PTy>,
        semi_opt: Option<TokenData>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: TokenData,
}
