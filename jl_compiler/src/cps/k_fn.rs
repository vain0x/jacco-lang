use super::{k_local::KLocalArena, KLabelArena, KLabelSigArena, KNode, KSymbol, KTy, KTyEnv, KVis};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KFnTag;

pub(crate) type KFn = VecArenaId<KFnTag>;

pub(crate) type KFnOutlineArena = VecArena<KFnTag, KFnOutline>;

pub(crate) type KFnArena = VecArena<KFnTag, KFnData>;

impl KFn {
    pub(crate) fn name(self, fns: &KFnOutlineArena) -> &str {
        &fns[self].name
    }

    pub(crate) fn param_tys(self, fns: &KFnOutlineArena) -> &[KTy] {
        &fns[self].param_tys
    }

    pub(crate) fn result_ty(self, fns: &KFnOutlineArena) -> &KTy {
        &fns[self].result_ty
    }

    pub(crate) fn return_ty(self, fns: &KFnOutlineArena) -> KTy {
        let result_ty = self.result_ty(fns).clone();
        KTy::Fn {
            param_tys: vec![result_ty],
            result_ty: Box::new(KTy::Never),
        }
    }

    pub(crate) fn ty(self, fns: &KFnOutlineArena) -> KTy {
        fns[self].ty()
    }

    pub(crate) fn is_pub(self, fns: &KFnOutlineArena) -> bool {
        fns[self].vis_opt.is_some()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KFnOutline {
    pub(crate) name: String,
    pub(crate) vis_opt: Option<KVis>,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) loc: Loc,
}

impl KFnOutline {
    pub(crate) fn ty(&self) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys.clone(),
            result_ty: Box::new(self.result_ty.clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: KLabelArena,
    pub(crate) label_sigs: KLabelSigArena,
    pub(crate) locals: KLocalArena,
    pub(crate) ty_env: KTyEnv,
}

impl KFnData {
    pub(crate) fn new(
        params: Vec<KSymbol>,
        body: KNode,
        locals: KLocalArena,
        labels: KLabelArena,
    ) -> Self {
        KFnData {
            params,
            body,
            labels,
            label_sigs: Default::default(),
            locals,
            ty_env: Default::default(),
        }
    }
}
