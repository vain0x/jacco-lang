use super::KTy;
use crate::source::Location;

#[derive(Clone, Debug)]
pub struct KFieldTag {
    pub name: String,
    pub location: Location,
}

#[derive(Clone, Copy, Debug)]
pub struct KField {
    id: usize,
}

impl KField {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, fields: &[KFieldOutline]) -> &str {
        &fields[self.id].name
    }

    pub fn ty(self, fields: &[KFieldOutline]) -> &KTy {
        &fields[self.id].ty
    }

    pub(crate) fn location(self, fields: &[KFieldOutline]) -> Location {
        fields[self.id].location.clone()
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
