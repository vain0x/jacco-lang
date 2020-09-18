use super::k_ty::{KTy2, KTyCause};
use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KLocalVarTag;

/// ローカル変数や仮引数
pub(crate) type KLocalVar = VecArenaId<KLocalVarTag>;

pub(crate) type KLocalVarArena = VecArena<KLocalVarTag, KLocalVarData>;

impl KLocalVar {
    pub(crate) fn name(self, local_vars: &KLocalVarArena) -> &str {
        &local_vars[self].name
    }

    pub(crate) fn ty(self, local_vars: &KLocalVarArena) -> KTy2 {
        local_vars[self].ty.clone()
    }
}

/// ローカル変数の親
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KLocalVarParent {
    Fn(KFn),
    ExternFn(KExternFn),
}

impl KLocalVarParent {
    #[cfg(unused)]
    pub(crate) fn local_vars(self, mod_data: &KModData) -> &KLocalVarArena {
        match self {
            KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).local_vars,
            KLocalVarParent::ExternFn(extern_fn) => &extern_fn.of(&mod_data.extern_fns).local_vars,
        }
    }

    #[cfg(unused)]
    pub(crate) fn labels(self, mod_data: &KModData) -> &KLabelArena {
        match self {
            KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).labels,
            KLocalVarParent::ExternFn(_) => KLabelArena::EMPTY,
        }
    }
}

#[derive(Clone)]
pub(crate) struct KLocalVarData {
    pub(crate) name: String,
    pub(crate) ty: KTy2,
    pub(crate) loc: Loc,
    pub(crate) is_alive: bool,
}

impl KLocalVarData {
    pub(crate) fn new(name: String, loc: Loc) -> Self {
        Self {
            name,
            ty: KTy2::Unresolved {
                cause: KTyCause::Loc(loc),
            },
            loc,
            is_alive: true,
        }
    }

    pub(crate) fn with_ty(mut self, ty: KTy2) -> Self {
        if !ty.is_unresolved() {
            assert!(self.ty.is_unresolved());
            self.ty = ty;
        }
        self
    }
}
