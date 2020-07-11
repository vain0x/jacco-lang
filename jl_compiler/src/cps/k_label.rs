use super::{KNode, KSymbol, KTy};
use crate::utils::{VecArena, VecArenaId};

pub(crate) struct KLabelTag;

pub(crate) type KLabel = VecArenaId<KLabelTag>;

pub(crate) type KLabelSigArena = VecArena<KLabelTag, KLabelSig>;

pub(crate) type KLabelArena = VecArena<KLabelTag, KLabelData>;

impl KLabel {
    pub(crate) fn ty(self, labels: &KLabelSigArena) -> KTy {
        labels[self].ty()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KLabelSig {
    name: String,
    param_tys: Vec<KTy>,
}

impl KLabelSig {
    pub(crate) fn new(name: String, param_tys: Vec<KTy>) -> Self {
        Self { name, param_tys }
    }

    pub(crate) fn ty(&self) -> KTy {
        let param_tys = self.param_tys.iter().cloned().collect();
        KTy::Fn {
            param_tys,
            result_ty: Box::new(KTy::Never),
        }
    }

    pub(crate) fn param_tys_mut(&mut self) -> &mut Vec<KTy> {
        &mut self.param_tys
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KLabelData {
    pub(crate) name: String,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
}
