use super::*;
use crate::{
    cps::k_const::KConstOutlineArena,
    source::Loc,
    utils::{VecArena, VecArenaId},
};
use k_const::KConsts;

pub(crate) struct KConstEnumTag;

pub(crate) type KConstEnum = VecArenaId<KConstEnumTag>;

pub(crate) type KConstEnumOutlines = VecArena<KConstEnumTag, KConstEnumOutline>;

impl KConstEnum {
    pub(crate) fn variants(self, const_enums: &KConstEnumOutlines) -> KConsts {
        self.of(const_enums).variants.clone()
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
    pub(crate) fn determine_tags(
        consts: &mut KConstOutlineArena,
        const_enums: &KConstEnumOutlines,
    ) {
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

    pub(crate) fn align_of(&self, mod_outline: &KModOutline) -> Option<usize> {
        self.repr_ty.align_of(KTyEnv::EMPTY, mod_outline)
    }

    pub(crate) fn size_of(&self, mod_outline: &KModOutline) -> Option<usize> {
        self.repr_ty.size_of(KTyEnv::EMPTY, mod_outline)
    }
}
