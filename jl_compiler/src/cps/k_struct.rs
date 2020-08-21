use super::{KField, KStructEnum, KStructEnumArena, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

#[derive(Clone, Debug)]
pub(crate) struct KStructParent {
    struct_enum: KStructEnum,

    /// これが何番目のバリアントか？
    index: usize,
}

impl KStructParent {
    pub(crate) fn new(struct_enum: KStructEnum, index: usize) -> Self {
        Self { struct_enum, index }
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

    pub(crate) fn tag_ty(self, structs: &KStructArena, struct_enums: &KStructEnumArena) -> KTy {
        match &structs[self].parent_opt {
            Some(parent) => parent.struct_enum.tag_ty(struct_enums),
            None => KTy::Unit,
        }
    }

    pub(crate) fn tag_value_opt(self, structs: &KStructArena) -> Option<usize> {
        Some(structs[self].parent_opt.as_ref()?.index)
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
