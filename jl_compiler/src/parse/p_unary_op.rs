/// 単項演算子
#[derive(Clone, Copy)]
pub(crate) enum PUnaryOp {
    /// `*`
    Deref,
    /// `&`
    Ref,
    /// `-`
    Minus,
    /// `!`
    Not,
}

impl PUnaryOp {
    /// 後ろに const/mut キーワードが続くか？
    pub(crate) fn allow_mut(self) -> bool {
        match self {
            PUnaryOp::Ref => true,
            _ => false,
        }
    }
}
