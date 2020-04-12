#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum BinaryOp {
    Assign,
    AddAssign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}
