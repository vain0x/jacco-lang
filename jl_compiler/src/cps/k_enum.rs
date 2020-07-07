use super::{KConst, KConstData, KConstValue, KStruct, KStructOutline, KTy};
use crate::token::{Location, TokenSource};

#[derive(Copy, Clone, Debug)]
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

    pub(crate) fn is_const_zero(self, consts: &[KConstData]) -> bool {
        self.as_const()
            .map_or(false, |k_const| k_const.is_zero(consts))
    }

    pub(crate) fn as_record(self) -> Option<KStruct> {
        match self {
            KVariant::Record(k_struct) => Some(k_struct),
            _ => None,
        }
    }

    pub(crate) fn as_record_with_name(
        self,
        name: &str,
        structs: &[KStructOutline],
    ) -> Option<KStruct> {
        self.as_record()
            .filter(|&k_struct| k_struct.name(structs) == name)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KEnum {
    id: usize,
}

impl KEnum {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, enums: &[KEnumOutline]) -> &str {
        &enums[self.id].name
    }

    #[allow(unused)]
    pub(crate) fn variants(self, enums: &[KEnumOutline]) -> &[KVariant] {
        &enums[self.id].variants
    }

    pub(crate) fn repr(self, enums: &[KEnumOutline]) -> &KEnumRepr {
        &enums[self.id].repr
    }

    pub(crate) fn is_tagged_union(self, enums: &[KEnumOutline]) -> bool {
        self.repr(enums).is_tagged_union()
    }

    pub(crate) fn tag_ty(self, enums: &[KEnumOutline]) -> &KTy {
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
    pub(crate) fn determine(variants: &[KVariant], consts: &[KConstData]) -> KEnumRepr {
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
    #[allow(unused)]
    pub(crate) fn keys(enums: &[KEnumOutline]) -> impl Iterator<Item = KEnum> {
        (0..enums.len()).map(KEnum::new)
    }

    #[allow(unused)]
    pub(crate) fn iter(enums: &[KEnumOutline]) -> impl Iterator<Item = (KEnum, &KEnumOutline)> {
        enums
            .iter()
            .enumerate()
            .map(|(i, enum_data)| (KEnum::new(i), enum_data))
    }

    pub(crate) fn determine_tags(
        consts: &mut [KConstData],
        enums: &mut [KEnumOutline],
        structs: &mut [KStructOutline],
    ) {
        for enum_data in enums {
            if !enum_data.repr.is_tagged_union() {
                // FIXME: const バリアントの未指定の値を埋める処理をここに移動する？
                continue;
            }

            for (i, variant) in enum_data.variants.iter().enumerate() {
                let tag = KConstValue::Usize(i);

                match variant {
                    KVariant::Const(k_const) => {
                        let old_value = k_const.value_opt_mut(consts).replace(tag);

                        // 構造体バリアントを持つ enum の const バリアントへの値の指定は許可されていないため
                        assert_eq!(old_value, None);
                    }
                    KVariant::Record(k_struct) => {
                        structs[k_struct.id()]
                            .parent_opt
                            .as_mut()
                            .unwrap()
                            .set_tag(tag);
                    }
                }
            }
        }
    }
}

impl Default for KEnumOutline {
    fn default() -> Self {
        KEnumOutline {
            name: Default::default(),
            variants: Default::default(),
            repr: Default::default(),
            location: Location::new(
                TokenSource::Special("<KEnumOutline::default>"),
                Default::default(),
            ),
        }
    }
}
