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

    pub(crate) fn value_opt(self, consts: &[KConstData]) -> Option<&KConstValue> {
        consts[self.id].value_opt.as_ref()
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KConstData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
}

#[derive(Clone, Debug)]
pub(crate) enum KConstValue {
    I32(i32),
    I64(i64),
    Usize(usize),
    F64(f64),
    Bool(bool),
}
