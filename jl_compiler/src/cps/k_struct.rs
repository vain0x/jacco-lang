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

impl KStructOutline {
    pub(crate) fn keys(structs: &[KStructOutline]) -> impl Iterator<Item = KStruct> {
        (0..structs.len()).map(KStruct::new)
    }

    pub(crate) fn iter(
        structs: &[KStructOutline],
    ) -> impl Iterator<Item = (KStruct, &KStructOutline)> {
        structs
            .iter()
            .enumerate()
            .map(|(i, struct_data)| (KStruct::new(i), struct_data))
    }
}
