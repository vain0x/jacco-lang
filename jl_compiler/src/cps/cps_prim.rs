#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum KPrim {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl KPrim {
    pub(crate) fn hint_str(self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}
