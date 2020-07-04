use super::{KConst, KStruct};
use crate::token::Location;

#[derive(Clone, Debug)]
pub(crate) enum KVariant {
    Const(KConst),
    Record(KStruct),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KEnum {
    id: usize,
}

impl KEnum {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, enums: &[KEnumOutline]) -> &str {
        &enums[self.id].name
    }

    #[allow(unused)]
    pub(crate) fn variants(self, enums: &[KEnumOutline]) -> &[KVariant] {
        &enums[self.id].variants
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KVariant>,
    pub(crate) location: Location,
}

impl KEnumOutline {
    #[allow(unused)]
    pub(crate) fn keys(enums: &[KEnumOutline]) -> impl Iterator<Item = KEnum> {
        (0..enums.len()).map(KEnum::new)
    }

    #[allow(unused)]
    pub(crate) fn iter(enums: &[KEnumOutline]) -> impl Iterator<Item = (KEnum, &KEnumOutline)> {
        enums
            .iter()
            .enumerate()
            .map(|(i, enum_data)| (KEnum::new(i), enum_data))
    }
}
