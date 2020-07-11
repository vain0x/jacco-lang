use super::{k_enum::KEnumArena, KConstValue, KEnum, KField, KTy};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};

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

pub(crate) struct KStructTag;

pub(crate) type KStruct = VecArenaId<KStructTag>;

pub(crate) type KStructArena = VecArena<KStructTag, KStructOutline>;

impl KStruct {
    pub(crate) fn name(self, structs: &KStructArena) -> &str {
        &structs[self].name
    }

    pub(crate) fn ty(self, structs: &KStructArena) -> KTy {
        match &structs[self].parent_opt {
            Some(parent) => KTy::Enum(parent.k_enum),
            None => KTy::Struct(self),
        }
    }

    pub(crate) fn tag_ty<'a>(self, structs: &KStructArena, enums: &'a KEnumArena) -> &'a KTy {
        match &structs[self].parent_opt {
            Some(parent) => parent.k_enum.tag_ty(enums),
            None => &KTy::Unit,
        }
    }

    pub(crate) fn tag_value_opt<'a>(self, structs: &KStructArena) -> Option<KConstValue> {
        structs[self].parent_opt.as_ref()?.tag_opt.clone()
    }

    pub(crate) fn fields(self, structs: &KStructArena) -> &[KField] {
        &structs[self].fields
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) parent_opt: Option<KStructParent>,
    pub(crate) location: Location,
}
