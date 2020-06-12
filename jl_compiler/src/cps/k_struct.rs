use super::{KField, KOutlines};
use crate::token::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KStruct {
    id: usize,
}

impl KStruct {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, outlines: &KOutlines) -> &str {
        &outlines.struct_get(self).name
    }

    pub(crate) fn fields(self, outlines: &KOutlines) -> &[KField] {
        &outlines.struct_get(self).fields
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) location: Location,
}
