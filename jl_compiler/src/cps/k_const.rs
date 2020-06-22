use super::KTy;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub(crate) struct KConst {
    id: usize,
}

impl KConst {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn ty(self, consts: &[KConstData]) -> &KTy {
        &consts[self.id].ty
    }

    pub(crate) fn value(self, consts: &[KConstData]) -> usize {
        consts[self.id].value
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KConstData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value: usize,
}
