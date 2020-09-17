use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KStructEnumTag;

pub(crate) type KStructEnum = VecArenaId<KStructEnumTag>;

pub(crate) type KStructEnumArena = VecArena<KStructEnumTag, KStructEnumOutline>;

impl KStructEnum {
    pub(crate) fn name(self, struct_enums: &KStructEnumArena) -> &str {
        &struct_enums[self].name
    }

    pub(crate) fn variants(self, struct_enums: &KStructEnumArena) -> &[KStruct] {
        &struct_enums[self].variants
    }

    pub(crate) fn tag_ty(self, struct_enums: &KStructEnumArena) -> KTy {
        struct_enums[self].tag_ty()
    }
}

#[derive(Clone)]
pub(crate) struct KStructEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KStruct>,
    pub(crate) loc: Loc,
}

impl KStructEnumOutline {
    pub(crate) fn tag_ty(&self) -> KTy {
        let variant_count = self.variants.len();
        if variant_count < u8::MAX as usize {
            KTy::U8
        } else if variant_count < u16::MAX as usize {
            KTy::U16
        } else {
            KTy::U32
        }
    }
}
