use super::*;
use crate::{
    cps::k_const::KConstArena,
    source::Loc,
    utils::{VecArena, VecArenaId},
};
use k_const::KConsts;

pub(crate) struct KConstEnumTag;

pub(crate) type KConstEnum = VecArenaId<KConstEnumTag>;

pub(crate) type KConstEnumOutlines = VecArena<KConstEnumTag, KConstEnumOutline>;

impl KConstEnum {
    pub(crate) fn name(self, const_enums: &KConstEnumOutlines) -> &str {
        &self.of(const_enums).name
    }

    #[allow(unused)]
    pub(crate) fn variants(self, const_enums: &KConstEnumOutlines) -> KConsts {
        self.of(const_enums).variants.clone()
    }

    pub(crate) fn repr_ty(self, const_enums: &KConstEnumOutlines) -> &KTy {
        &self.of(const_enums).repr_ty
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct KProjectConstEnum(pub(crate) KMod, pub(crate) KConstEnum);

impl KProjectConstEnum {
    pub(crate) fn of(self, mod_outlines: &KModOutlines) -> &KConstEnumOutline {
        let KProjectConstEnum(k_mod, const_enum) = self;
        const_enum.of(&k_mod.of(mod_outlines).const_enums)
    }
}

#[derive(Clone)]
pub(crate) struct KConstEnumOutline {
    pub(crate) name: String,
    pub(crate) repr_ty: KTy,
    pub(crate) variants: KConsts,
    pub(crate) loc: Loc,
}

impl KConstEnumOutline {
    pub(crate) fn determine_tags(consts: &mut KConstArena, const_enums: &KConstEnumOutlines) {
        for data in const_enums.iter() {
            let mut tag = 0;
            for k_const in data.variants.iter() {
                if let Some(value) = &k_const.of(consts).value_opt {
                    tag = value.cast_as_usize() + 1;
                    continue;
                }

                k_const.of_mut(consts).value_opt = Some(KConstValue::Usize(tag));
                tag += 1;
            }
        }
    }
}
