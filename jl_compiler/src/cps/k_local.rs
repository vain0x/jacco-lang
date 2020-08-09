use super::k_ty::{KTy2, KTyCause};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KLocalTag;

/// ローカル変数や仮引数
pub(crate) type KLocal = VecArenaId<KLocalTag>;

pub(crate) type KLocalArena = VecArena<KLocalTag, KLocalData>;

impl KLocal {
    pub(crate) fn name(self, locals: &KLocalArena) -> &str {
        &locals[self].name
    }

    pub(crate) fn ty(self, locals: &KLocalArena) -> KTy2 {
        locals[self].ty.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KLocalData {
    pub(crate) name: String,
    pub(crate) ty: KTy2,
    pub(crate) loc: Loc,
    pub(crate) is_alive: bool,
}

impl KLocalData {
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
