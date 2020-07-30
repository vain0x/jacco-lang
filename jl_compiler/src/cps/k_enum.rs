use super::{k_const::KConstArena, k_struct::KStructArena, KConst, KConstValue, KStruct, KTy};
use crate::{
    source::Loc,
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
        match self {
            KVariant::Const(k_const) => Some(k_const),
            _ => None,
        }
    }

    pub(crate) fn is_const(self) -> bool {
        self.as_const().is_some()
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

    pub(crate) fn is_unit_like(self, consts: &KConstArena) -> bool {
        match self {
            KVariant::Const(k_const) => !k_const.has_value(consts),
            KVariant::Record(_) => {
                // FIXME: unit-like record なら true
                false
            }
        }
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

pub(crate) type KEnumReprs = VecArena<KEnumTag, KEnumRepr>;

impl KEnum {
    pub(crate) fn name(self, enums: &KEnumArena) -> &str {
        &enums[self].name
    }

    #[allow(unused)]
    pub(crate) fn variants(self, enums: &KEnumArena) -> &[KVariant] {
        &enums[self].variants
    }

    pub(crate) fn repr(self, enums: &KEnumReprs) -> &KEnumRepr {
        &enums[self]
    }

    pub(crate) fn is_tagged_union(self, enums: &KEnumReprs) -> bool {
        self.repr(enums).is_tagged_union()
    }

    pub(crate) fn tag_ty(self, enums: &KEnumReprs) -> &KTy {
        match &self.repr(enums) {
            KEnumRepr::Never => &KTy::Never,
            KEnumRepr::Unit => &KTy::Unit,
            KEnumRepr::Const { value_ty } => value_ty,
            KEnumRepr::TaggedUnion { tag_ty } => tag_ty,
        }
    }
}

/// enum 型のコンパイル後の表現
#[derive(Clone, Debug)]
pub(crate) enum KEnumRepr {
    Never,
    Unit,
    Const { value_ty: KTy },
    TaggedUnion { tag_ty: KTy },
}

impl KEnumRepr {
    pub(crate) fn determine(variants: &[KVariant], consts: &KConstArena) -> KEnumRepr {
        match variants {
            [] => KEnumRepr::Never,
            [variant] if variant.is_unit_like(consts) => KEnumRepr::Unit,
            _ if variants.iter().all(|variant| variant.is_const()) => KEnumRepr::Const {
                // FIXME: 値を見て決定する
                value_ty: KTy::USIZE,
            },
            _ => {
                // FIXME: たいていの場合は u8 で十分
                KEnumRepr::TaggedUnion { tag_ty: KTy::USIZE }
            }
        }
    }

    pub(crate) fn is_tagged_union(&self) -> bool {
        match self {
            KEnumRepr::TaggedUnion { .. } => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct KEnumOutline {
    pub(crate) name: String,
    pub(crate) variants: Vec<KVariant>,
    pub(crate) loc: Loc,
}

impl KEnumOutline {
    pub(crate) fn determine_tags(
        consts: &mut KConstArena,
        enums: &mut KEnumArena,
        enum_reprs: &KEnumReprs,
        structs: &mut KStructArena,
    ) {
        for (enum_data, repr) in enums.iter_mut().zip(enum_reprs.iter()) {
            match repr {
                KEnumRepr::Never => continue,
                KEnumRepr::Unit => {
                    for &variant in enum_data.variants.iter() {
                        let tag = KConstValue::Usize(0);
                        match variant {
                            KVariant::Const(k_const) => {
                                k_const.of_mut(consts).value_opt = Some(tag)
                            }
                            KVariant::Record(k_struct) => k_struct
                                .of_mut(structs)
                                .parent_opt
                                .as_mut()
                                .unwrap()
                                .set_tag(tag),
                        }
                    }
                    continue;
                }
                KEnumRepr::Const { .. } => {
                    let mut tag = 0;
                    for &variant in enum_data.variants.iter() {
                        let k_const = variant.as_const().unwrap();
                        if let Some(value) = &k_const.of(consts).value_opt {
                            tag = value.cast_as_usize() + 1;
                            continue;
                        }

                        k_const.of_mut(consts).value_opt = Some(KConstValue::Usize(tag));
                        tag += 1;
                    }
                    continue;
                }
                KEnumRepr::TaggedUnion { .. } => {}
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
