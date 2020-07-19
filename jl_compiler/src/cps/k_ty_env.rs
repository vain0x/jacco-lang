use super::{k_meta_ty::KMetaTys, KMetaTy, KMetaTyData};
use crate::token::Location;
use std::cell::RefCell;

#[derive(Clone, Debug, Default)]
pub(crate) struct KTyEnv {
    meta_tys: KMetaTys,
}

impl KTyEnv {
    pub(crate) const EMPTY: &'static KTyEnv = &KTyEnv {
        meta_tys: KMetaTys::new(),
    };

    pub(crate) fn is_empty(&self) -> bool {
        self.meta_tys.is_empty()
    }

    pub(crate) fn meta_ty_new(&mut self, location: Location) -> KMetaTy {
        self.meta_tys
            .alloc(KMetaTyData::new(RefCell::default(), location))
    }

    pub(crate) fn meta_ty_get(&self, meta_ty: KMetaTy) -> &KMetaTyData {
        &self.meta_tys[meta_ty]
    }
}
