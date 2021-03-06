use super::*;
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
            ty_params: fns[self].ty_params.clone(),
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

#[derive(Clone)]
pub(crate) struct KFnOutline {
    pub(crate) name: String,
    pub(crate) vis_opt: Option<KVis>,
    pub(crate) ty_params: Vec<KTyParam>,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) loc: Loc,
}

impl KFnOutline {
    pub(crate) fn ty(&self) -> KTy {
        KTy::Fn {
            ty_params: self.ty_params.clone(),
            param_tys: self.param_tys.clone(),
            result_ty: Box::new(self.result_ty.clone()),
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct KFnData {
    pub(crate) params: Vec<KVarTerm>,
    pub(crate) labels: KLabelArena,
    pub(crate) label_sigs: KLabelSigArena,
    pub(crate) local_vars: KLocalVarArena,
    pub(crate) ty_env: KTyEnv,
}

impl KFnData {
    pub(crate) fn new(
        params: Vec<KVarTerm>,
        local_vars: KLocalVarArena,
        labels: KLabelArena,
        ty_env: KTyEnv,
    ) -> Self {
        KFnData {
            params,
            labels,
            label_sigs: Default::default(),
            local_vars,
            ty_env,
        }
    }
}
