use super::{KNode, KSymbol, KTy};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct KLabel {
    id: usize,
}

impl KLabel {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn ty(self, labels: &[KLabelSig]) -> KTy {
        labels[self.id].ty()
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
