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

impl CBinaryOp {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            CBinaryOp::Assign => "=",
            CBinaryOp::Add => "+",
            CBinaryOp::Sub => "-",
            CBinaryOp::Mul => "*",
            CBinaryOp::Div => "/",
            CBinaryOp::Modulo => "%",
            CBinaryOp::BitAnd => "&",
            CBinaryOp::BitOr => "|",
            CBinaryOp::BitXor => "^",
            CBinaryOp::LeftShift => "<<",
            CBinaryOp::RightShift => ">>",
            CBinaryOp::Eq => "==",
            CBinaryOp::Ne => "!=",
            CBinaryOp::Lt => "<",
            CBinaryOp::Le => "<=",
            CBinaryOp::Gt => ">",
            CBinaryOp::Ge => ">=",
        }
    }
}
