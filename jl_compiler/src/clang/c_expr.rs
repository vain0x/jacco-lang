use super::{CBinaryOp, CStmt, CTy, CUnaryOp};

pub(crate) enum CExpr {
    IntLit(String),
    LongLongLit(String),
    CharLit(String),
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
    Cast {
        ty: CTy,
        arg: Box<CExpr>,
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

    pub(crate) fn into_cast(self, ty: CTy) -> CExpr {
        CExpr::Cast {
            ty,
            arg: Box::new(self),
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
