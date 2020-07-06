use super::{KEnum, KEnumOutline, KField, KTy};
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

    pub(crate) fn name(self, structs: &[KStructOutline]) -> &str {
        &structs[self.id].name
    }

    pub(crate) fn ty(self, structs: &[KStructOutline]) -> KTy {
        match structs[self.id].parent_enum_opt {
            Some(k_enum) => KTy::Enum(k_enum),
            None => KTy::Struct(self),
        }
    }

    pub(crate) fn tag_ty<'a>(
        self,
        structs: &[KStructOutline],
        enums: &'a [KEnumOutline],
    ) -> &'a KTy {
        match structs[self.id].parent_enum_opt {
            Some(k_enum) => k_enum.tag_ty(enums),
            None => &KTy::Unit,
        }
    }

    pub(crate) fn fields(self, structs: &[KStructOutline]) -> &[KField] {
        &structs[self.id].fields
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) parent_enum_opt: Option<KEnum>,
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
