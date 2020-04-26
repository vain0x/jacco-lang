#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum KPrim {
    If,
    Let,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl KPrim {
    pub(crate) fn hint_str(self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}
