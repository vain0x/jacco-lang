use super::{k_ty::KTy2, KTy};
use crate::{
    token::Location,
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

    pub(crate) fn ty(self, locals: &KLocalArena) -> KTy {
        locals[self].ty.to_ty1()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KLocalData {
    pub(crate) name: String,
    pub(crate) ty: KTy2,
    pub(crate) location: Location,
    pub(crate) is_alive: bool,
}

impl KLocalData {
    pub(crate) fn new(name: String, location: Location) -> Self {
        Self {
            name,
            ty: KTy2::Unresolved,
            location,
            is_alive: true,
        }
    }

    pub(crate) fn with_ty(mut self, ty: KTy2) -> Self {
        if ty != KTy2::Unresolved {
            assert_eq!(self.ty, KTy2::Unresolved);
            self.ty = ty;
        }
        self
    }
}
