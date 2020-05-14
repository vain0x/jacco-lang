//! C言語の構文木の定義

pub(crate) enum CUnaryOp {
    Deref,
    Ref,
    Minus,
    Negate,
}

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
    Other(&'static str),
    Void,
    Int,
    Ptr { ty: Box<CTy> },
    Struct(String),
}

pub(crate) enum CExpr {
    IntLit(String),
    Name(String),
    #[allow(dead_code)]
    Call {
        cal: Box<CExpr>,
        args: Vec<CExpr>,
    },
    UnaryOp {
        op: CUnaryOp,
        arg: Box<CExpr>,
    },
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
    StructDecl {
        name: String,
    },
}

pub(crate) struct CRoot {
    pub(crate) body: Vec<CStmt>,
}
