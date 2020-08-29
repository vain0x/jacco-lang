use super::k_ty::{KTy2, KTyCause};
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

#[derive(Clone, Debug)]
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
