#![allow(unused)]

use super::*;
use crate::{
    cps::k_const::KConstArena,
    source::Loc,
    utils::{VecArena, VecArenaId},
};
use k_const::KConsts;

pub(crate) struct KConstEnumTag;

pub(crate) type KConstEnum = VecArenaId<KConstEnumTag>;

pub(crate) type KConstEnumArena = VecArena<KConstEnumTag, KConstEnumOutline>;

impl KConstEnum {
    pub(crate) fn name(self, const_enums: &KConstEnumArena) -> &str {
        &self.of(const_enums).name
    }

    #[allow(unused)]
    pub(crate) fn variants(self, const_enums: &KConstEnumArena) -> KConsts {
        self.of(const_enums).variants.clone()
    }

    pub(crate) fn repr_ty(self, const_enums: &KConstEnumArena) -> &KTy {
        &self.of(const_enums).repr_ty
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
    pub(crate) fn determine_tags(consts: &mut KConstArena, const_enums: &mut KConstEnumArena) {
        for data in const_enums.iter_mut() {
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
