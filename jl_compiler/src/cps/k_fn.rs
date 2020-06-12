use super::{KLabelData, KLabelSig, KLocalData, KNode, KOutlines, KSymbol, KTy, KTyEnv};
use crate::token::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct KFn {
    id: usize,
}

impl KFn {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, outlines: &KOutlines) -> &str {
        &outlines.fn_get(self).name
    }

    pub(crate) fn param_tys(self, outlines: &KOutlines) -> &[KTy] {
        &outlines.fn_get(self).param_tys
    }

    pub(crate) fn result_ty(self, outlines: &KOutlines) -> &KTy {
        &outlines.fn_get(self).result_ty
    }

    pub(crate) fn return_ty(self, outlines: &KOutlines) -> KTy {
        let result_ty = self.result_ty(outlines).clone();
        KTy::Fn {
            param_tys: vec![result_ty],
            result_ty: Box::new(KTy::Never),
        }
    }

    pub(crate) fn ty(self, outlines: &KOutlines) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(outlines).to_owned(),
            result_ty: Box::new(self.result_ty(outlines).clone()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KLabelData>,
    pub(crate) label_sigs: Vec<KLabelSig>,
    pub(crate) locals: Vec<KLocalData>,
    pub(crate) ty_env: KTyEnv,
}
