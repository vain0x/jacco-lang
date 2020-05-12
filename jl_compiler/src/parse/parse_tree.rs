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
pub(crate) struct PNameTy(pub(crate) PName);

#[derive(Clone, Debug)]
pub(crate) struct PNeverTy {
    pub(crate) bang: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PUnitTy {
    pub(crate) left: TokenData,
    pub(crate) right_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PPtrTy {
    pub(crate) star: TokenData,
    pub(crate) ty_opt: Option<Box<PTy>>,
}

#[derive(Clone, Debug)]
pub(crate) enum PTy {
    Name(PNameTy),
    Never(PNeverTy),
    Unit(PUnitTy),
    Ptr(PPtrTy),
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
pub(crate) struct PIntExpr {
    pub(crate) token: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PStrExpr {
    pub(crate) token: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PNameExpr(pub(crate) PName);

#[derive(Clone, Debug)]
pub(crate) struct PTupleExpr {
    pub(crate) arg_list: PArgList,
}

#[derive(Clone, Debug)]
pub(crate) struct PCallExpr {
    pub(crate) callee: Box<PExpr>,
    pub(crate) arg_list: PArgList,
}

#[derive(Clone, Debug)]
pub(crate) struct PUnaryOpExpr {
    pub(crate) op: PUnaryOp,
    pub(crate) arg_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) struct PBinaryOpExpr {
    pub(crate) op: PBinaryOp,
    pub(crate) left: Box<PExpr>,
    pub(crate) right_opt: Option<Box<PExpr>>,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) struct PBlockExpr(pub(crate) PBlock);

#[derive(Clone, Debug)]
pub(crate) struct PBreakExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PContinueExpr {
    pub(crate) keyword: TokenData,
}

#[derive(Clone, Debug)]
pub(crate) struct PReturnExpr {
    pub(crate) keyword: TokenData,
    pub(crate) arg_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PIfExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
    pub(crate) else_opt: Option<TokenData>,
    pub(crate) alt_opt: Option<Box<PExpr>>,
}

#[derive(Clone, Debug)]
pub(crate) struct PWhileExpr {
    pub(crate) keyword: TokenData,
    pub(crate) cond_opt: Option<Box<PExpr>>,
    pub(crate) body_opt: Option<PBlock>,
}

#[derive(Clone, Debug)]
pub(crate) struct PLoopExpr {
    pub(crate) keyword: TokenData,
    pub(crate) body_opt: Option<PBlock>,
}

#[derive(Clone, Debug)]
pub(crate) enum PExpr {
    Int(PIntExpr),
    Str(PStrExpr),
    Name(PNameExpr),
    Tuple(PTupleExpr),
    Call(PCallExpr),
    UnaryOp(PUnaryOpExpr),
    BinaryOp(PBinaryOpExpr),
    Block(PBlockExpr),
    Break(PBreakExpr),
    Continue(PContinueExpr),
    Return(PReturnExpr),
    If(PIfExpr),
    While(PWhileExpr),
    Loop(PLoopExpr),
}

impl Default for PExpr {
    fn default() -> Self {
        PExpr::Str(PStrExpr {
            token: TokenData::default(),
        })
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PExprDecl {
    pub(crate) expr: PExpr,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PLetDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) colon_opt: Option<TokenData>,
    pub(crate) ty_opt: Option<PTy>,
    pub(crate) equal_opt: Option<TokenData>,
    pub(crate) init_opt: Option<PExpr>,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) struct PFnDecl {
    pub(crate) keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) result_opt: Option<PTy>,
    pub(crate) block_opt: Option<PBlock>,
}

#[derive(Clone, Debug)]
pub(crate) struct PExternFnDecl {
    pub(crate) extern_keyword: TokenData,
    pub(crate) fn_keyword: TokenData,
    pub(crate) name_opt: Option<PName>,
    pub(crate) param_list_opt: Option<PParamList>,
    pub(crate) arrow_opt: Option<TokenData>,
    pub(crate) result_opt: Option<PTy>,
    pub(crate) semi_opt: Option<TokenData>,
}

#[derive(Clone, Debug)]
pub(crate) enum PDecl {
    Expr(PExprDecl),
    Let(PLetDecl),
    Fn(PFnDecl),
    ExternFn(PExternFnDecl),
}

#[derive(Clone, Debug)]
pub(crate) struct PRoot {
    pub(crate) decls: Vec<PDecl>,
    pub(crate) eof: TokenData,
}
