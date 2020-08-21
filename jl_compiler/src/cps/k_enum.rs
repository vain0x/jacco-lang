use super::{k_const::KConstArena, k_struct::KStructArena, KConst, KConstValue, KStruct, KTy};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};
use std::fmt::{self, Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) enum KVariant {
    #[allow(unused)]
    Const(KConst),
    Record(KStruct),
}

impl KVariant {
    pub(crate) fn as_record(self) -> Option<KStruct> {
        match self {
            KVariant::Record(k_struct) => Some(k_struct),
            _ => None,
        }
    }

    pub(crate) fn as_record_with_name(self, name: &str, structs: &KStructArena) -> Option<KStruct> {
        self.as_record()
            .filter(|&k_struct| k_struct.name(structs) == name)
    }

    #[allow(unused)]
    pub(crate) fn name<'a>(self, consts: &'a KConstArena, structs: &'a KStructArena) -> &'a str {
        match self {
            KVariant::Const(k_const) => &k_const.of(consts).name,
            KVariant::Record(k_struct) => &k_struct.of(structs).name,
        }
    }
}

impl Debug for KVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            KVariant::Const(inner) => Debug::fmt(inner, f),
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
    pub(crate) fn determine_tags(
        consts: &mut KConstArena,
        enums: &mut KEnumArena,
        structs: &mut KStructArena,
    ) {
        for enum_data in enums.iter_mut() {
            for (i, &variant) in enum_data.variants.iter().enumerate() {
                let tag = KConstValue::Usize(i);

                match variant {
                    KVariant::Const(k_const) => {
                        let old_value = k_const.of_mut(consts).value_opt.replace(tag);

                        // 構造体バリアントを持つ enum の const バリアントへの値の指定は許可されていないため
                        assert_eq!(old_value, None);
                    }
                    KVariant::Record(k_struct) => {
                        structs[k_struct].parent_opt.as_mut().unwrap().set_tag(tag);
                    }
                }
            }
        }
    }
}
