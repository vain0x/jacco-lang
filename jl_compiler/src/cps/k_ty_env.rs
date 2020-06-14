use super::{KMetaTy, KMetaTyData, KTy};
use crate::token::Location;
use std::cell::RefCell;

#[derive(Clone, Debug, Default)]
pub(crate) struct KTyEnv {
    meta_tys: Vec<KMetaTyData>,
}

impl KTyEnv {
    pub(crate) fn is_empty(&self) -> bool {
        self.meta_tys.is_empty()
    }

    pub(crate) fn meta_ty_new(&mut self, location: Location) -> KMetaTy {
        let id = self.meta_tys.len();
        self.meta_tys
            .push(KMetaTyData::new(RefCell::default(), location));
        KMetaTy::new(id)
    }

    pub(crate) fn meta_ty_get(&self, meta_ty: KMetaTy) -> &KMetaTyData {
        &self.meta_tys[meta_ty.id()]
    }

    pub(crate) fn is_unbound(&self, ty: &KTy) -> bool {
        match ty {
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(self) {
                Some(ty) => self.is_unbound(&*ty.borrow()),
                None => true,
            },
            _ => false,
        }
    }

    pub(crate) fn is_unit_or_never(&self, ty: &KTy) -> bool {
        match ty {
            KTy::Unit | KTy::Never => true,
            KTy::Meta(meta_ty) => match meta_ty.try_unwrap(self) {
                Some(ty) => self.is_unit_or_never(&*ty.borrow()),
                None => false,
            },
            _ => false,
        }
    }
}
