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

    pub(crate) fn value_opt_mut(self, consts: &mut [KConstData]) -> &mut Option<KConstValue> {
        &mut consts[self.id].value_opt
    }

    pub(crate) fn is_zero(self, consts: &[KConstData]) -> bool {
        match &consts[self.id].value_opt {
            Some(value) => value.is_zero(),
            None => true,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KConstData {
    pub(crate) name: String,
    /// 定数の型、または定数バリアントが所属する enum の型
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum KConstValue {
    I32(i32),
    I64(i64),
    Usize(usize),
    F64(f64),
    Bool(bool),
}

impl KConstValue {
    pub(crate) fn is_zero(&self) -> bool {
        self.cast_as_usize() == 0
    }

    pub(crate) fn cast_as_usize(&self) -> usize {
        match self {
            KConstValue::I32(value) => *value as usize,
            KConstValue::I64(value) => *value as usize,
            KConstValue::Usize(value) => *value,
            KConstValue::F64(value) => *value as usize,
            KConstValue::Bool(value) => *value as usize,
        }
    }

    #[allow(unused)]
    pub(crate) fn ty(&self) -> KTy {
        match self {
            KConstValue::I32(_) => KTy::I32,
            KConstValue::I64(_) => KTy::I64,
            KConstValue::Usize(_) => KTy::Usize,
            KConstValue::F64(_) => KTy::F64,
            KConstValue::Bool(_) => KTy::Bool,
        }
    }
}
