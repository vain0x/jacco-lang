use super::{KNode, KSymbol, KTy};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KLabel {
    id: usize,
}

impl KLabel {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, label_sigs: &[KLabelSig]) -> &str {
        &label_sigs[self.id].name
    }

    pub fn ty(self, label_sigs: &[KLabelSig]) -> KTy {
        let param_tys = label_sigs[self.id].param_tys.iter().cloned().collect();
        KTy::Fn {
            param_tys,
            result_ty: Box::new(KTy::Never),
        }
    }

    pub fn param_tys_mut(self, label_sigs: &mut [KLabelSig]) -> &mut Vec<KTy> {
        &mut label_sigs[self.id].param_tys
    }

    pub fn params(self, labels: &[KLabelData]) -> &[KSymbol] {
        &labels[self.id].params
    }

    pub fn body(self, labels: &[KLabelData]) -> &KNode {
        &labels[self.id].body
    }
}

#[derive(Clone, Debug)]
pub struct KLabelSig {
    name: String,
    param_tys: Vec<KTy>,
}

impl KLabelSig {
    pub(crate) fn new(name: String, param_tys: Vec<KTy>) -> Self {
        Self { name, param_tys }
    }
}

#[derive(Clone, Debug)]
pub struct KLabelData {
    pub(crate) name: String,
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
}

impl KLabelData {
    pub fn iter(labels: &[KLabelData]) -> impl Iterator<Item = (KLabel, &str, &[KSymbol], &KNode)> {
        labels.iter().enumerate().map(|(i, data)| {
            (
                KLabel::new(i),
                data.name.as_str(),
                data.params.as_slice(),
                &data.body,
            )
        })
    }
}
