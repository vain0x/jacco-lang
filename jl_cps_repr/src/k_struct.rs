use super::KField;
use crate::source::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KStruct {
    id: usize,
}

impl KStruct {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub fn id(self) -> usize {
        self.id
    }

    pub fn name(self, structs: &[KStructOutline]) -> &str {
        &structs[self.id].name
    }

    pub fn fields(self, structs: &[KStructOutline]) -> &[KField] {
        &structs[self.id].fields
    }
}

#[derive(Clone, Debug)]
pub struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) location: Location,
}

impl KStructOutline {
    pub fn keys(structs: &[KStructOutline]) -> impl Iterator<Item = KStruct> {
        (0..structs.len()).map(KStruct::new)
    }
}
