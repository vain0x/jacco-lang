/// 二項演算子
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum PBinaryOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModuloAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    LeftShiftAssign,
    RightShiftAssign,
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
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    LogAnd,
    LogOr,
}
