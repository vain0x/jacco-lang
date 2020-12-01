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

    pub(crate) fn align_of(&self, mod_outline: &KModOutline) -> Option<usize> {
        let mut align = self.tag_ty().align_of(KTyEnv::EMPTY, mod_outline)?;

        for variant in &self.variants {
            let variant_align = variant.of(&mod_outline.structs).do_align_of(mod_outline)?;
            align = align.max(variant_align);
        }

        Some(align)
    }

    pub(crate) fn size_of(&self, mod_outline: &KModOutline) -> Option<usize> {
        let align = self.align_of(mod_outline)?;

        let mut tag_size = self.tag_ty().size_of(KTyEnv::EMPTY, mod_outline)?;
        if tag_size % align != 0 {
            tag_size += align - tag_size % align;
        }

        let mut variant_size = 0;
        for variant in &self.variants {
            let mut size = variant.of(&mod_outline.structs).do_size_of(mod_outline)?;
            if size % align != 0 {
                size += align - size % align;
            }

            variant_size = variant_size.max(size);
        }

        Some(tag_size + variant_size)
    }
}
