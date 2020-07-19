use super::k_ty::KTy2;
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

pub(crate) type KTyEnv = VecArena<KMetaTyTag, KMetaTyData>;

impl KMetaTy {
    pub(crate) fn try_unwrap(self, ty_env: &KTyEnv) -> Option<&RefCell<KTy2>> {
        let cell = ty_env[self].ty();
        if *cell.borrow() == KTy2::Unresolved {
            return None;
        }

        Some(cell)
    }

    pub(crate) fn bind(self, new_ty: KTy2, ty_env: &KTyEnv) {
        debug_assert!(self.try_unwrap(ty_env).is_none());
        debug_assert_ne!(new_ty, KTy2::Unresolved);

        let data = &ty_env[self];
        let old = replace(&mut *data.ty().borrow_mut(), new_ty);

        // バインドは1回のみ。
        debug_assert_eq!(old, KTy2::Unresolved);
    }
}

#[derive(Clone)]
pub(crate) struct KMetaTyData {
    /// 型推論の単一化において、メタ型変数への参照を持ちながら他のメタ型変数への束縛を行う必要があるので、
    /// おそらく RefCell を避けるのは難しい。
    ty: RefCell<KTy2>,

    location: Location,
}

impl KMetaTyData {
    pub(crate) fn new(ty: RefCell<KTy2>, location: Location) -> Self {
        Self { ty, location }
    }

    pub(crate) fn ty(&self) -> &RefCell<KTy2> {
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

impl KTyEnv {
    pub(crate) const EMPTY: &'static Self = &Self::new();
}
