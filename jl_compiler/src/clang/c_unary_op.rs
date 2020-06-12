pub(crate) enum CUnaryOp {
    Deref,
    Ref,
    Minus,
    Not,
}

impl CUnaryOp {
    pub(crate) fn as_str(&self) -> &'static str {
        match self {
            CUnaryOp::Deref => "*",
            CUnaryOp::Ref => "&",
            CUnaryOp::Minus => "-",
            CUnaryOp::Not => "!",
        }
    }
}
