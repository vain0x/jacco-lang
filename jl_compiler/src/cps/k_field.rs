use super::KTy;
use crate::token::{Location, TokenSource};

#[derive(Clone, Debug)]
pub(crate) struct KFieldTag {
    pub(crate) name: String,
    pub(crate) location: Location,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) struct KField {
    id: usize,
}

impl KField {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, fields: &[KFieldOutline]) -> &str {
        &fields[self.id].name
    }

    pub(crate) fn ty(self, fields: &[KFieldOutline]) -> &KTy {
        &fields[self.id].ty
    }

    pub(crate) fn location(self, fields: &[KFieldOutline]) -> Location {
        fields[self.id].location.clone()
    }
}

#[derive(Debug)]
pub(crate) struct KFieldOutline {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) location: Location,
}

impl Default for KFieldOutline {
    fn default() -> Self {
        Self {
            name: Default::default(),
            ty: Default::default(),
            location: Location::new(
                TokenSource::Special("<KFieldOutline::default>"),
                Default::default(),
            ),
        }
    }
}
