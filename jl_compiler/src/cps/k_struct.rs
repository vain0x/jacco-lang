use super::{KConstValue, KField, KStructEnum, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

#[derive(Clone, Debug)]
pub(crate) struct KStructParent {
    struct_enum: KStructEnum,
    tag_opt: Option<KConstValue>,
}

impl KStructParent {
    pub(crate) fn new(struct_enum: KStructEnum) -> Self {
        Self {
            struct_enum,
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
            Some(parent) => KTy::StructEnum(parent.struct_enum),
            None => KTy::Struct(self),
        }
    }

    pub(crate) fn tag_ty<'a>(self, structs: &KStructArena) -> &'a KTy {
        match &structs[self].parent_opt {
            Some(parent) => parent.struct_enum.tag_ty(),
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
    pub(crate) loc: Loc,
}

impl KStructOutline {
    pub(crate) fn is_unit_like(&self) -> bool {
        self.fields.is_empty()
    }
}
