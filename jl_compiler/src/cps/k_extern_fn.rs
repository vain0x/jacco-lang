use super::*;
use crate::{
    source::Loc,
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
        extern_fns[self].ty()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct KProjectExternFn(pub(crate) KMod, pub(crate) KExternFn);

impl KProjectExternFn {
    pub(crate) fn k_mod(self) -> KMod {
        self.0
    }

    pub(crate) fn of(self, mod_outlines: &KModOutlines) -> &KExternFnOutline {
        let KProjectExternFn(k_mod, extern_fn) = self;
        extern_fn.of(&k_mod.of(mod_outlines).extern_fns)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KExternFnOutline {
    pub(crate) name: String,
    pub(crate) param_tys: Vec<KTy>,
    pub(crate) result_ty: KTy,
    pub(crate) loc: Loc,
}

impl KExternFnOutline {
    pub(crate) fn ty(&self) -> KTy {
        KTy::Fn {
            param_tys: self.param_tys.clone(),
            result_ty: Box::new(self.result_ty.clone()),
        }
    }
}

#[derive(Clone, Default, Debug)]
pub(crate) struct KExternFnData {
    pub(crate) params: Vec<KSymbol>,
    pub(crate) locals: KLocalArena,
}
