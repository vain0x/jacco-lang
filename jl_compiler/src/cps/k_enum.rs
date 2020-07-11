use super::{k_const::KConstArena, k_struct::KStructArena, KConst, KConstValue, KStruct, KTy};
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};
use std::fmt::{self, Debug, Formatter};

#[derive(Copy, Clone)]
pub(crate) enum KVariant {
    Const(KConst),
    Record(KStruct),
}

impl KVariant {
    pub(crate) fn as_const(self) -> Option<KConst> {
        if let KVariant::Const(k_const) = self {
            Some(k_const)
        } else {
            None
        }
    }

    pub(crate) fn is_const(self) -> bool {
        self.as_const().is_some()
    }

    pub(crate) fn is_const_zero(self, consts: &KConstArena) -> bool {
        self.as_const()
            .map_or(false, |k_const| k_const.is_zero(consts))
    }

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

    pub(crate) fn repr(self, enums: &KEnumArena) -> &KEnumRepr {
        &enums[self].repr
    }

    pub(crate) fn is_tagged_union(self, enums: &KEnumArena) -> bool {
        self.repr(enums).is_tagged_union()
    }

    pub(crate) fn tag_ty(self, enums: &KEnumArena) -> &KTy {
        match self.repr(enums) {
            KEnumRepr::Never => &KTy::Never,
            KEnumRepr::Unit => &KTy::Unit,
            KEnumRepr::Const { value_ty } => value_ty,
            KEnumRepr::Sum { tag_ty } => tag_ty,
        }
    }
}

/// enum 型のコンパイル後の表現
#[derive(Clone, Debug)]
pub(crate) enum KEnumRepr {
    Never,
    Unit,
    Const {
        value_ty: KTy,
    },
    /// tagged union
    Sum {
        tag_ty: KTy,
    },
}

impl KEnumRepr {
    pub(crate) fn determine(variants: &[KVariant], consts: &KConstArena) -> KEnumRepr {
        match variants {
            [] => KEnumRepr::Never,
            [variant] if variant.is_const_zero(consts) => KEnumRepr::Unit,
            _ if variants.iter().all(|variant| variant.is_const()) => KEnumRepr::Const {
                // FIXME: 値を見て決定する
                value_ty: KTy::Usize,
            },
            _ => {
                // FIXME: たいていの場合は u8 で十分
                KEnumRepr::Sum { tag_ty: KTy::Usize }
            }
        }
    }

    pub(crate) fn is_tagged_union(&self) -> bool {
        match self {
            KEnumRepr::Sum { .. } => true,
            _ => false,
        }
    }
}

impl Default for KEnumRepr {
    fn default() -> Self {
        KEnumRepr::Never
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KVariant>,
    pub(crate) repr: KEnumRepr,
    pub(crate) location: Location,
}

impl KEnumOutline {
    pub(crate) fn determine_tags(
        consts: &mut KConstArena,
        enums: &mut KEnumArena,
        structs: &mut KStructArena,
    ) {
        for enum_data in enums.iter_mut() {
            if !enum_data.repr.is_tagged_union() {
                // FIXME: const バリアントの未指定の値を埋める処理をここに移動する？
                continue;
            }

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
