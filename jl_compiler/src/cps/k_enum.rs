use super::{k_struct::KStructArena, KConstValue, KStruct, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KEnumTag;

pub(crate) type KEnum = VecArenaId<KEnumTag>;

pub(crate) type KEnumArena = VecArena<KEnumTag, KEnumOutline>;

impl KEnum {
    pub(crate) fn name(self, enums: &KEnumArena) -> &str {
        &enums[self].name
    }

    pub(crate) fn variants(self, enums: &KEnumArena) -> &[KStruct] {
        &enums[self].variants
    }

    pub(crate) fn tag_ty(self) -> &'static KTy {
        &KTy::USIZE
    }
}

#[derive(Clone)]
pub(crate) struct KEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KStruct>,
    pub(crate) loc: Loc,
}

impl KEnumOutline {
    pub(crate) fn determine_tags(enums: &mut KEnumArena, structs: &mut KStructArena) {
        for enum_data in enums.iter_mut() {
            for (i, &k_struct) in enum_data.variants.iter().enumerate() {
                let tag = KConstValue::Usize(i);
                structs[k_struct].parent_opt.as_mut().unwrap().set_tag(tag);
            }
        }
    }
}
