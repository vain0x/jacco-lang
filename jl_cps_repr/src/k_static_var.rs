use super::{KConstValue, KTy};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct KStaticVar {
    id: usize,
}

impl KStaticVar {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, static_vars: &[KStaticVarData]) -> &str {
        &static_vars[self.id].name
    }

    pub fn ty(self, static_vars: &[KStaticVarData]) -> &KTy {
        &static_vars[self.id].ty
    }

    pub fn init_opt(self, static_vars: &[KStaticVarData]) -> Option<&KConstValue> {
        static_vars[self.id].value_opt.as_ref()
    }
}

#[derive(Clone, Debug, Default)]
pub struct KStaticVarData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
}

impl KStaticVarData {
    pub fn iter(
        static_vars: &[KStaticVarData],
    ) -> impl Iterator<Item = (KStaticVar, &str, &KTy, Option<&KConstValue>)> {
        static_vars.iter().enumerate().map(|(i, data)| {
            (
                KStaticVar::new(i),
                data.name.as_str(),
                &data.ty,
                data.value_opt.as_ref(),
            )
        })
    }
}
