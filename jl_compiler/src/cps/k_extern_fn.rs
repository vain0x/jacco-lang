use super::{KLocalData, KOutlines, KSymbol, KTy};
use crate::token::Location;

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

    pub(crate) fn name(self, outlines: &KOutlines) -> &str {
        &outlines.extern_fn_get(self).name
    }

    pub(crate) fn param_tys(self, outlines: &KOutlines) -> &[KTy] {
        &outlines.extern_fn_get(self).param_tys
    }

    pub(crate) fn result_ty(self, outlines: &KOutlines) -> &KTy {
        &outlines.extern_fn_get(self).result_ty
    }

    pub(crate) fn ty(self, outlines: &KOutlines) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(outlines).to_owned(),
            result_ty: Box::new(self.result_ty(outlines).clone()),
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

#[derive(Clone, Debug)]
pub(crate) struct KExternFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) locals: Vec<KLocalData>,
}
