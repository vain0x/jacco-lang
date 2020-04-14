pub(crate) enum CTy {
    Void,
    Bool,
    Int,
    Enum {
        ident: String,
        variants: Option<Vec<(String, CTy)>>,
    },
}

pub(crate) enum CExpr {
    BoolLit(bool),
    IntLit(String),
    Name(String),
    Call { cal: Box<CExpr>, args: Vec<CExpr> },
    Neg(Box<CExpr>),
    Add(Box<CExpr>, Box<CExpr>),
}

pub(crate) struct CBlock {
    pub(crate) body: Vec<CStmt>,
}

pub(crate) enum CStmt {
    Expr(CExpr),
    Block(CBlock),
    Return(Option<CExpr>),
    If {
        cond: CExpr,
        body: Box<CStmt>,
    },
    VarDecl {
        name: String,
        ty: CTy,
        init_opt: Option<CExpr>,
    },
    FnDecl {
        ident: String,
        params: Vec<(String, CTy)>,
        result_ty: CTy,
        body: CBlock,
    },
}

pub(crate) struct CRoot {
    pub(crate) body: Vec<CStmt>,
}
