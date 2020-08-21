use super::{k_struct::KStructArena, KConstValue, KStruct, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};
use std::fmt::{self, Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) enum KVariant {
    Record(KStruct),
}

impl KVariant {
    pub(crate) fn as_record(self) -> KStruct {
        match self {
            KVariant::Record(k_struct) => k_struct,
        }
    }

    pub(crate) fn name(self, structs: &KStructArena) -> &str {
        match self {
            KVariant::Record(k_struct) => &k_struct.of(structs).name,
        }
    }
}

impl Debug for KVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KVariant::Record(inner) => Debug::fmt(inner, f),
        }
    }
}

pub(crate) struct KEnumTag;

pub(crate) type KEnum = VecArenaId<KEnumTag>;

pub(crate) type KEnumArena = VecArena<KEnumTag, KEnumOutline>;

impl KEnum {
    pub(crate) fn name(self, enums: &KEnumArena) -> &str {
        &enums[self].name
    }

    #[allow(unused)]
    pub(crate) fn variants(self, enums: &KEnumArena) -> &[KVariant] {
        &enums[self].variants
    }

    pub(crate) fn tag_ty(self) -> &'static KTy {
        &KTy::USIZE
    }
}

#[derive(Clone)]
pub(crate) struct KEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KVariant>,
    pub(crate) loc: Loc,
}

impl KEnumOutline {
    pub(crate) fn determine_tags(enums: &mut KEnumArena, structs: &mut KStructArena) {
        for enum_data in enums.iter_mut() {
            for (i, &variant) in enum_data.variants.iter().enumerate() {
                let tag = KConstValue::Usize(i);

                match variant {
                    KVariant::Record(k_struct) => {
                        structs[k_struct].parent_opt.as_mut().unwrap().set_tag(tag);
                    }
                }
            }
        }
    }
}
