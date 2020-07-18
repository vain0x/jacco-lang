use super::{KTy, KTyEnv};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};
use std::{
    cell::RefCell,
    fmt::{self, Debug, Formatter},
    mem::replace,
};

pub(crate) struct KMetaTyTag;

pub(crate) type KMetaTy = VecArenaId<KMetaTyTag>;

pub(crate) type KMetaTys = VecArena<KMetaTyTag, KMetaTyData>;

impl KMetaTy {
    pub(crate) fn try_unwrap(self, ty_env: &KTyEnv) -> Option<&RefCell<KTy>> {
        if ty_env.meta_ty_get(self).ty().borrow().is_unresolved() {
            return None;
        }

        Some(ty_env.meta_ty_get(self).ty())
    }

    pub(crate) fn bind(&mut self, new_ty: KTy, ty_env: &KTyEnv) {
        debug_assert!(self.try_unwrap(ty_env).is_none());
        debug_assert!(!new_ty.is_unresolved());

        let data = ty_env.meta_ty_get(*self);
        let old = replace(&mut *data.ty().borrow_mut(), new_ty);

        debug_assert!(old.is_unresolved());
    }
}

#[derive(Clone)]
pub(crate) struct KMetaTyData {
    /// 型推論の単一化において、メタ型変数への参照を持ちながら他のメタ型変数への束縛を行う必要があるので、
    /// おそらく RefCell を避けるのは難しい。
    ty: RefCell<KTy>,

    location: Location,
}

impl KMetaTyData {
    pub(crate) fn new(ty: RefCell<KTy>, location: Location) -> Self {
        Self { ty, location }
    }

    pub(crate) fn ty(&self) -> &RefCell<KTy> {
        &self.ty
    }
}

impl Debug for KMetaTyData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // let p = self.ty.as_ptr() as usize;
        // match &*self.ty.borrow() {
        //     KTy::Unresolved => write!(f, "?<{}>", p),
        //     ty => Debug::fmt(ty, f),
        // }
        // FIXME: need env
        Debug::fmt(&self.ty, f)
    }
}
