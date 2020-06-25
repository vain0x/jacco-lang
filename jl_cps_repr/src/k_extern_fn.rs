use super::{KLocalData, KSymbol, KTy};
use crate::source::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KExternFn {
    id: usize,
}

impl KExternFn {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, extern_fns: &[KExternFnOutline]) -> &str {
        &extern_fns[self.id].name
    }

    pub fn param_tys(self, extern_fns: &[KExternFnOutline]) -> &[KTy] {
        &extern_fns[self.id].param_tys
    }

    pub fn result_ty(self, extern_fns: &[KExternFnOutline]) -> &KTy {
        &extern_fns[self.id].result_ty
    }

    pub fn ty(self, extern_fns: &[KExternFnOutline]) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(extern_fns).to_owned(),
            result_ty: Box::new(self.result_ty(extern_fns).clone()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct KExternFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

#[derive(Clone, Debug, Default)]
pub struct KExternFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) locals: Vec<KLocalData>,
}

impl KExternFnData {
    pub fn iter(
        extern_fns: &[KExternFnData],
    ) -> impl Iterator<Item = (KExternFn, &[KSymbol], &[KLocalData])> {
        extern_fns.iter().enumerate().map(|(i, data)| {
            (
                KExternFn::new(i),
                data.params.as_slice(),
                data.locals.as_slice(),
            )
        })
    }
}
