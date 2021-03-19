use super::k_ty::KTy2;
use crate::utils::{VecArena, VecArenaId};
use std::{cell::RefCell, mem::replace};

pub(crate) struct KMetaTyTag;

pub(crate) type KMetaTy = VecArenaId<KMetaTyTag>;

pub(crate) type KTyEnv = VecArena<KMetaTyTag, KMetaTyData>;

impl KMetaTy {
    pub(crate) fn try_unwrap(self, ty_env: &KTyEnv) -> Option<&RefCell<KTy2>> {
        let cell = ty_env[self].ty();
        if cell.borrow().is_unresolved() {
            return None;
        }

        Some(cell)
    }

    pub(crate) fn bind(self, new_ty: KTy2, ty_env: &KTyEnv) {
        debug_assert!(self.try_unwrap(ty_env).is_none());
        debug_assert!(!new_ty.is_unresolved());

        let data = &ty_env[self];
        let old = replace(&mut *data.ty().borrow_mut(), new_ty);

        // バインドは1回のみ。
        debug_assert!(old.is_unresolved());
    }
}

pub(crate) struct KMetaTyData {
    // NOTE: 型推論の単一化において、メタ型変数への参照を持ちながら他のメタ型変数への束縛を行う必要があるので、
    //       おそらく RefCell を避けるのは難しい。
    ty: RefCell<KTy2>,
}

impl KMetaTyData {
    pub(crate) fn new(ty: RefCell<KTy2>) -> Self {
        Self { ty }
    }

    pub(crate) fn new_fresh() -> Self {
        Self {
            ty: RefCell::default(),
        }
    }

    pub(crate) fn ty(&self) -> &RefCell<KTy2> {
        &self.ty
    }
}
