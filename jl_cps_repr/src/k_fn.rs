use super::{KLabelData, KLabelSig, KLocalData, KNode, KSymbol, KTy, KTyEnv, KVis};
use crate::source::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct KFn {
    id: usize,
}

impl KFn {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, fns: &[KFnOutline]) -> &str {
        &fns[self.id].name
    }

    pub fn is_pub(self, fns: &[KFnOutline]) -> bool {
        fns[self.id].vis_opt.is_some()
    }

    pub fn param_tys(self, fns: &[KFnOutline]) -> &[KTy] {
        &fns[self.id].param_tys
    }

    pub fn result_ty(self, fns: &[KFnOutline]) -> &KTy {
        &fns[self.id].result_ty
    }

    pub fn return_ty(self, fns: &[KFnOutline]) -> KTy {
        let result_ty = self.result_ty(fns).clone();
        KTy::Fn {
            param_tys: vec![result_ty],
            result_ty: Box::new(KTy::Never),
        }
    }

    pub fn ty(self, fns: &[KFnOutline]) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(fns).to_owned(),
            result_ty: Box::new(self.result_ty(fns).clone()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct KFnOutline {
    pub(crate) name: String,
    pub(crate) vis_opt: Option<KVis>,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

#[derive(Clone, Debug, Default)]
pub struct KFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) body: KNode,
    pub(crate) labels: Vec<KLabelData>,
    pub(crate) label_sigs: Vec<KLabelSig>,
    pub(crate) locals: Vec<KLocalData>,
    pub(crate) ty_env: KTyEnv,
}

impl KFnData {
    pub fn iter(
        fns: &[KFnData],
    ) -> impl Iterator<
        Item = (
            KFn,
            &[KSymbol],
            &KNode,
            &[KLabelData],
            &[KLabelSig],
            &[KLocalData],
            &KTyEnv,
        ),
    > {
        fns.iter().enumerate().map(|(i, data)| {
            (
                KFn::new(i),
                data.params.as_slice(),
                &data.body,
                data.labels.as_slice(),
                data.label_sigs.as_slice(),
                data.locals.as_slice(),
                &data.ty_env,
            )
        })
    }
}
