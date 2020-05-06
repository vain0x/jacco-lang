//! C言語の構文木の定義

pub(crate) enum CBinaryOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub(crate) enum CTy {
    #[allow(dead_code)]
    Void,
    Int,
}

pub(crate) enum CExpr {
    IntLit(String),
    Name(String),
    #[allow(dead_code)]
    Call {
        cal: Box<CExpr>,
        args: Vec<CExpr>,
    },
    #[allow(dead_code)]
    Neg(Box<CExpr>),
    BinaryOp {
        op: CBinaryOp,
        left: Box<CExpr>,
        right: Box<CExpr>,
    },
}

pub(crate) struct CBlock {
    pub(crate) body: Vec<CStmt>,
}

pub(crate) enum CStmt {
    #[allow(dead_code)]
    Expr(CExpr),
    #[allow(dead_code)]
    Block(CBlock),
    Label {
        label: String,
    },
    Goto {
        label: String,
    },
    Return(Option<CExpr>),
    #[allow(dead_code)]
    If {
        cond: CExpr,
        body: Box<CStmt>,
        alt: Box<CStmt>,
    },
    VarDecl {
        name: String,
        ty: CTy,
        init_opt: Option<CExpr>,
    },
    FnDecl {
        name: String,
        params: Vec<(String, CTy)>,
        result_ty: CTy,
        body: CBlock,
    },
    ExternFnDecl {
        name: String,
        params: Vec<(String, CTy)>,
        result_ty: CTy,
    },
}

pub(crate) struct CRoot {
    pub(crate) body: Vec<CStmt>,
}
