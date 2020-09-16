use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

#[derive(Clone, Debug)]
pub(crate) enum KStructParent {
    Enum {
        struct_enum: KStructEnum,

        /// これが何番目のバリアントか？
        index: usize,
    },
    Struct {
        ty_params: Vec<KTyParam>,
    },
}

impl KStructParent {
    pub(crate) fn new_enum(struct_enum: KStructEnum, index: usize) -> Self {
        Self::Enum { struct_enum, index }
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
        match structs[self].parent {
            KStructParent::Enum { struct_enum, .. } => KTy::StructEnum(struct_enum),
            KStructParent::Struct { .. } => KTy::Struct(self),
        }
    }

    pub(crate) fn tag_ty(self, structs: &KStructArena, struct_enums: &KStructEnumArena) -> KTy {
        match structs[self].parent {
            KStructParent::Enum { struct_enum, .. } => struct_enum.tag_ty(struct_enums),
            KStructParent::Struct { .. } => KTy::Unit,
        }
    }

    pub(crate) fn tag_value_opt(self, structs: &KStructArena) -> Option<usize> {
        match structs[self].parent {
            KStructParent::Enum { index, .. } => Some(index),
            KStructParent::Struct { .. } => None,
        }
    }

    pub(crate) fn fields(self, structs: &KStructArena) -> &[KField] {
        &structs[self].fields
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    pub(crate) fields: Vec<KField>,
    pub(crate) parent: KStructParent,
    pub(crate) loc: Loc,
}

impl KStructOutline {
    pub(crate) fn is_unit_like(&self) -> bool {
        self.fields.is_empty()
    }

    pub(crate) fn ty_params(&self) -> &[KTyParam] {
        match &self.parent {
            KStructParent::Enum { .. } => &[],
            KStructParent::Struct { ty_params } => ty_params.as_ref(),
        }
    }
}
