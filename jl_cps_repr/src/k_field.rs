use super::{KOutlines, KTy};
use crate::source::Location;

#[derive(Clone, Debug)]
pub struct KFieldTag {
    pub(crate) name: String,
    pub(crate) location: Location,
}

#[derive(Clone, Copy, Debug)]
pub struct KField {
    id: usize,
}

impl KField {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, outlines: &KOutlines) -> &str {
        &outlines.field_get(self).name
    }

    pub(crate) fn ty(self, outlines: &KOutlines) -> &KTy {
        &outlines.field_get(self).ty
    }

    pub(crate) fn location(self, outlines: &KOutlines) -> Location {
        outlines.field_get(self).location.clone()
    }
}

#[derive(Debug)]
pub struct KFieldOutline {
    name: String,
    ty: KTy,
    location: Location,
}

impl KFieldOutline {
    pub(crate) fn new(name: String, ty: KTy, location: Location) -> Self {
        Self { name, ty, location }
    }
}
