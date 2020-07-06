use super::{KConstValue, KEnum, KEnumOutline, KField, KTy};
use crate::token::Location;

#[derive(Clone, Debug)]
pub(crate) struct KStructParent {
    k_enum: KEnum,
    tag_opt: Option<KConstValue>,
}

impl KStructParent {
    pub(crate) fn new(k_enum: KEnum) -> Self {
        Self {
            k_enum,
            tag_opt: None,
        }
    }

    pub(crate) fn set_tag(&mut self, tag: KConstValue) {
        let old_value = self.tag_opt.replace(tag);

        assert_eq!(old_value, None);
    }
}

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
        match &structs[self.id].parent_opt {
            Some(parent) => KTy::Enum(parent.k_enum),
            None => KTy::Struct(self),
        }
    }

    pub(crate) fn tag_ty<'a>(
        self,
        structs: &[KStructOutline],
        enums: &'a [KEnumOutline],
    ) -> &'a KTy {
        match &structs[self.id].parent_opt {
            Some(parent) => parent.k_enum.tag_ty(enums),
            None => &KTy::Unit,
        }
    }

    pub(crate) fn tag_value_opt<'a>(self, structs: &[KStructOutline]) -> Option<KConstValue> {
        structs[self.id].parent_opt.as_ref()?.tag_opt.clone()
    }

    pub(crate) fn fields(self, structs: &[KStructOutline]) -> &[KField] {
        &structs[self.id].fields
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) parent_opt: Option<KStructParent>,
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
