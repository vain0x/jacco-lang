/// CPS 中間表現のプリミティブの種類
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum KPrim {
    /// 行き詰まり
    Stuck,
    Jump,
    CallDirect,
    // enum のタグを取り出す
    #[allow(unused)]
    EnumToTag,
    Record,
    /// 構造体へのポインタから特定のフィールドへのポインタを取得する。(`&p->field`)
    GetField,
    GetFieldMut,
    If,
    Switch,
    Let,
    Deref,
    Ref,
    RefMut,
    Minus,
    Not,
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
    Cast,
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
}

impl KPrim {
    pub(crate) fn hint_str(self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}

impl Default for KPrim {
    fn default() -> Self {
        KPrim::Stuck
    }
}
