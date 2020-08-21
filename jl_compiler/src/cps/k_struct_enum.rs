use super::{k_struct::KStructArena, KConstValue, KStruct, KTy};
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

    pub(crate) fn tag_ty(self) -> &'static KTy {
        &KTy::USIZE
    }
}

#[derive(Clone)]
pub(crate) struct KStructEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KStruct>,
    pub(crate) loc: Loc,
}

impl KStructEnumOutline {
    pub(crate) fn determine_tags(struct_enums: &mut KStructEnumArena, structs: &mut KStructArena) {
        for data in struct_enums.iter_mut() {
            for (i, &k_struct) in data.variants.iter().enumerate() {
                let tag = KConstValue::Usize(i);
                structs[k_struct].parent_opt.as_mut().unwrap().set_tag(tag);
            }
        }
    }
}
