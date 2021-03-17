use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KStaticVarTag;

pub(crate) type KStaticVar = VecArenaId<KStaticVarTag>;

pub(crate) type KStaticVarOutlineArena = VecArena<KStaticVarTag, KStaticVarOutline>;

impl KStaticVar {
    pub(crate) fn name(self, static_vars: &KStaticVarOutlineArena) -> &str {
        &static_vars[self].name
    }

    pub(crate) fn ty(self, static_vars: &KStaticVarOutlineArena) -> &KTy {
        &static_vars[self].ty
    }
}

pub(crate) struct KStaticVarOutline {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
    pub(crate) loc: Loc,
}

pub(crate) type KStaticVarInits = VecArena<KStaticVarTag, KStaticVarInit>;

pub(crate) struct KStaticVarInit {
    pub(crate) init_opt: Option<(KNode, KTerm)>,
}

impl KStaticVarInit {
    pub(crate) fn new_empty() -> Self {
        Self { init_opt: None }
    }
}
