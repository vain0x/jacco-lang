use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KStaticVarTag;

pub(crate) type KStaticVar = VecArenaId<KStaticVarTag>;

pub(crate) type KStaticVarArena = VecArena<KStaticVarTag, KStaticVarOutline>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct KProjectStaticVar(pub(crate) KMod, pub(crate) KStaticVar);

impl KStaticVar {
    pub(crate) fn name(self, static_vars: &KStaticVarArena) -> &str {
        &static_vars[self].name
    }

    pub(crate) fn ty(self, static_vars: &KStaticVarArena) -> &KTy {
        &static_vars[self].ty
    }
}

impl KProjectStaticVar {
    pub(crate) fn k_mod(self) -> KMod {
        self.0
    }

    pub(crate) fn of(self, mod_outlines: &KModOutlines) -> &KStaticVarOutline {
        let KProjectStaticVar(k_mod, static_var) = self;
        static_var.of(&k_mod.of(mod_outlines).static_vars)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KStaticVarOutline {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
    pub(crate) loc: Loc,
}

pub(crate) type KStaticVarInits = VecArena<KStaticVarTag, KStaticVarInit>;

#[derive(Clone, Debug)]
pub(crate) struct KStaticVarInit {
    pub(crate) init_opt: Option<(KNode, KTerm)>,
}

impl KStaticVarInit {
    pub(crate) fn new_empty() -> Self {
        Self { init_opt: None }
    }
}
