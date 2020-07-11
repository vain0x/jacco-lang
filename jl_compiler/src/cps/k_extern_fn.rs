use super::{KLocalArena, KSymbol, KTy};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KExternFnTag;

pub(crate) type KExternFn = VecArenaId<KExternFnTag>;

pub(crate) type KExternFnOutlineArena = VecArena<KExternFnTag, KExternFnOutline>;

pub(crate) type KExternFnArena = VecArena<KExternFnTag, KExternFnData>;

impl KExternFn {
    pub(crate) fn name(self, extern_fns: &KExternFnOutlineArena) -> &str {
        &extern_fns[self].name
    }

    pub(crate) fn param_tys(self, extern_fns: &KExternFnOutlineArena) -> &[KTy] {
        &extern_fns[self].param_tys
    }

    pub(crate) fn result_ty(self, extern_fns: &KExternFnOutlineArena) -> &KTy {
        &extern_fns[self].result_ty
    }

    pub(crate) fn ty(self, extern_fns: &KExternFnOutlineArena) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys(extern_fns).to_owned(),
            result_ty: Box::new(self.result_ty(extern_fns).clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) location: Location,
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) locals: KLocalArena,
}
