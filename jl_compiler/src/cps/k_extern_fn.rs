use super::{KLocalData, KSymbol, KTy};
use crate::token::{Location, TokenSource};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct KExternFn {
    id: usize,
}

impl KExternFn {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, extern_fns: &[KExternFnOutline]) -> &str {
        &extern_fns[self.id].name
    }

    pub(crate) fn param_tys(self, extern_fns: &[KExternFnOutline]) -> &[KTy] {
        &extern_fns[self.id].param_tys
    }

    pub(crate) fn result_ty(self, extern_fns: &[KExternFnOutline]) -> &KTy {
        &extern_fns[self.id].result_ty
    }

    pub(crate) fn ty(self, extern_fns: &[KExternFnOutline]) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(extern_fns).to_owned(),
            result_ty: Box::new(self.result_ty(extern_fns).clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

impl Default for KExternFnOutline {
    fn default() -> Self {
        KExternFnOutline {
            name: Default::default(),
            param_tys: Default::default(),
            result_ty: Default::default(),
            location: Location::new(
                TokenSource::Special("<KExternFnOutline::default>"),
                Default::default(),
            ),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KExternFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) locals: Vec<KLocalData>,
}

impl KExternFnData {
    pub(crate) fn iter(fns: &[KExternFnData]) -> impl Iterator<Item = (KExternFn, &KExternFnData)> {
        fns.iter()
            .enumerate()
            .map(|(i, fn_data)| (KExternFn::new(i), fn_data))
    }

    pub(crate) fn iter_mut(
        fns: &mut [KExternFnData],
    ) -> impl Iterator<Item = (KExternFn, &mut KExternFnData)> {
        fns.iter_mut()
            .enumerate()
            .map(|(i, fn_data)| (KExternFn::new(i), fn_data))
    }
}
