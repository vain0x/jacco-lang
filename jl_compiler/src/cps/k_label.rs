use super::{k_ty::KTy2, KNode, KVarTerm};
use crate::utils::{VecArena, VecArenaId};

pub(crate) struct KLabelTag;

pub(crate) type KLabel = VecArenaId<KLabelTag>;

pub(crate) type KLabelSigArena = VecArena<KLabelTag, KLabelSig>;

pub(crate) type KLabelArena = VecArena<KLabelTag, KLabelData>;

impl KLabel {
    pub(crate) fn ty(self, labels: &KLabelSigArena) -> KTy2 {
        labels[self].ty()
    }
}

#[derive(Clone)]
pub(crate) struct KLabelSig {
    name: String,
    param_tys: Vec<KTy2>,
}

impl KLabelSig {
    pub(crate) fn new(name: String, param_tys: Vec<KTy2>) -> Self {
        Self { name, param_tys }
    }

    pub(crate) fn ty(&self) -> KTy2 {
        KTy2::new_fn(self.param_tys.clone(), KTy2::Never)
    }

    pub(crate) fn param_tys_mut(&mut self) -> &mut Vec<KTy2> {
        &mut self.param_tys
    }
}

#[derive(Clone)]
pub(crate) struct KLabelData {
    pub(crate) name: String,
    pub(crate) parent_opt: Option<KLabel>,
    pub(crate) params: Vec<KVarTerm>,
    pub(crate) body: KNode,
}
