//! C言語の構文木の定義

pub(crate) enum CUnaryOp {
    Deref,
    Ref,
    Minus,
    Not,
}

pub(crate) enum CBinaryOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
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

impl CTy {
    pub(crate) fn into_ptr(self) -> CTy {
        CTy::Ptr { ty: Box::new(self) }
    }
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
        left: Box<CExpr>,
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

    pub(crate) fn into_call(self, args: impl Iterator<Item = CExpr>) -> CExpr {
        CExpr::Call {
            left: Box::new(self),
            args: args.collect(),
        }
    }

    pub(crate) fn into_unary_op(self, unary_op: CUnaryOp) -> CExpr {
        CExpr::UnaryOp {
            op: unary_op,
            arg: Box::new(self),
        }
    }

    pub(crate) fn into_binary_op(self, binary_op: CBinaryOp, right: CExpr) -> CExpr {
        CExpr::BinaryOp {
            op: binary_op,
            left: Box::new(self),
            right: Box::new(right),
        }
    }

    pub(crate) fn into_stmt(self) -> CStmt {
        CStmt::Expr(self)
    }
}

pub(crate) struct CBlock {
    pub(crate) stmts: Vec<CStmt>,
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
    pub(crate) decls: Vec<CStmt>,
}
