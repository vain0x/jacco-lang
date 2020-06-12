use super::{KMetaTy, KMetaTyData};
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
}
