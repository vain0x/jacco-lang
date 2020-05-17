/// CPS 中間表現のプリミティブの種類
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum KPrim {
    Stuck,
    Jump,
    CallDirect,
    Struct,
    GetField,
    If,
    Let,
    Deref,
    Ref,
    Minus,
    Negate,
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
    Assign,
}

impl KPrim {
    pub(crate) fn hint_str(self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}
