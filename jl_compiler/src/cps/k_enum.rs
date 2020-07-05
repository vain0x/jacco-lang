use super::{KConst, KConstData, KStruct, KTy};
use crate::token::Location;

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
                value_ty: KTy::Usize,
            },
            _ => KEnumRepr::Sum { tag_ty: KTy::Usize },
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

#[derive(Clone, Debug, Default)]
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
}
