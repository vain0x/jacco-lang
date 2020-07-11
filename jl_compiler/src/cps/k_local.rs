use super::KTy;
use crate::utils::{VecArena, VecArenaId};

pub(crate) struct KLocalTag;

/// ローカル変数や仮引数
pub(crate) type KLocal = VecArenaId<KLocalTag>;

pub(crate) type KLocalArena = VecArena<KLocalTag, KLocalData>;

impl KLocal {
    pub(crate) fn name(self, locals: &KLocalArena) -> &str {
        &locals[self].name
    }

    pub(crate) fn ty(self, locals: &KLocalArena) -> &KTy {
        &locals[self].ty
    }

    pub(crate) fn ty_mut(self, locals: &mut KLocalArena) -> &mut KTy {
        &mut locals[self].ty
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KLocalData {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) is_alive: bool,
}
