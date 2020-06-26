use super::{KLabelData, KLabelSig, KLocalData, KNode, KSymbol, KTy, KTyEnv};
use crate::parse::Vis;
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

    pub(crate) fn name(self, fns: &[KFnOutline]) -> &str {
        &fns[self.id].name
    }

    pub(crate) fn param_tys(self, fns: &[KFnOutline]) -> &[KTy] {
        &fns[self.id].param_tys
    }

    pub(crate) fn result_ty(self, fns: &[KFnOutline]) -> &KTy {
        &fns[self.id].result_ty
    }

    pub(crate) fn return_ty(self, fns: &[KFnOutline]) -> KTy {
        let result_ty = self.result_ty(fns).clone();
        KTy::Fn {
            param_tys: vec![result_ty],
            result_ty: Box::new(KTy::Never),
        }
    }

    pub(crate) fn ty(self, fns: &[KFnOutline]) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(fns).to_owned(),
            result_ty: Box::new(self.result_ty(fns).clone()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KFnOutline {
    pub(crate) name: String,
    pub(crate) vis_opt: Option<Vis>,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

impl KFnOutline {
    pub(crate) fn is_pub(&self) -> bool {
        self.vis_opt.is_some()
    }
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

impl KFnData {
    pub(crate) fn into_iter(
        fns: impl IntoIterator<Item = KFnData>,
    ) -> impl Iterator<Item = (KFn, KFnData)> {
        fns.into_iter()
            .enumerate()
            .map(|(i, fn_data)| (KFn::new(i), fn_data))
    }

    pub(crate) fn iter_mut(fns: &mut [KFnData]) -> impl Iterator<Item = (KFn, &mut KFnData)> {
        fns.iter_mut()
            .enumerate()
            .map(|(i, fn_data)| (KFn::new(i), fn_data))
    }
}
