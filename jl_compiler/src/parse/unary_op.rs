/// 単項演算子
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum PUnaryOp {
    /// `*`
    Deref,
    /// `&`
    Ref,
    /// `-`
    Minus,
    /// `!`
    Negate,
}
