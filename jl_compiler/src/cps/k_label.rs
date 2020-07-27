use super::{k_ty::KTy2, KNode, KSymbol};
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub(crate) struct KLabelData {
    pub(crate) name: String,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
}

impl KLabelData {
    pub(crate) fn new(name: String) -> Self {
        Self {
            name,
            params: vec![],
            body: KNode::default(),
        }
    }
}
