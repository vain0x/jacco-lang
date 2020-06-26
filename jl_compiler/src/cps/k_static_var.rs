use super::{KConstValue, KTy};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct KStaticVar {
    id: usize,
}

impl KStaticVar {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, static_vars: &[KStaticVarData]) -> &str {
        &static_vars[self.id].name
    }

    pub(crate) fn ty(self, static_vars: &[KStaticVarData]) -> &KTy {
        &static_vars[self.id].ty
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KStaticVarData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
}
