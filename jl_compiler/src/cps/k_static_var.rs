use super::{KConstValue, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KStaticVarTag;

pub(crate) type KStaticVar = VecArenaId<KStaticVarTag>;

pub(crate) type KStaticVarArena = VecArena<KStaticVarTag, KStaticVarData>;

impl KStaticVar {
    pub(crate) fn name(self, static_vars: &KStaticVarArena) -> &str {
        &static_vars[self].name
    }

    pub(crate) fn ty(self, static_vars: &KStaticVarArena) -> &KTy {
        &static_vars[self].ty
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KStaticVarData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) value_opt: Option<KConstValue>,
    pub(crate) location: Loc,
}
