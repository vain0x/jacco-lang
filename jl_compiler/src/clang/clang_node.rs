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
    Dot {
        left: Box<CExpr>,
        field: String,
    },
    Arrow {
        left: Box<CExpr>,
        field: String,
    },
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

impl CExpr {
    pub(crate) fn into_ref(self) -> CExpr {
        CExpr::UnaryOp {
            op: CUnaryOp::Ref,
            arg: Box::new(self),
        }
    }

    pub(crate) fn into_dot(self, field: impl Into<String>) -> CExpr {
        CExpr::Dot {
            left: Box::new(self),
            field: field.into(),
        }
    }

    pub(crate) fn into_arrow(self, field: impl Into<String>) -> CExpr {
        CExpr::Arrow {
            left: Box::new(self),
            field: field.into(),
        }
    }
}

pub(crate) struct CBlock {
    pub(crate) body: Vec<CStmt>,
}

pub(crate) enum CStmt {
    Expr(CExpr),
    Block(CBlock),
    Label {
        label: String,
    },
    Goto {
        label: String,
    },
    Return(Option<CExpr>),
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
        fields: Vec<(String, CTy)>,
    },
}

pub(crate) struct CRoot {
    pub(crate) body: Vec<CStmt>,
}
