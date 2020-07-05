use super::{KEnumOutline, KField, KTy};
use crate::token::Location;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct KStruct {
    id: usize,
}

impl KStruct {
    pub(crate) fn new(id: usize) -> Self {
        Self { id }
    }

    pub(crate) fn id(self) -> usize {
        self.id
    }

    pub(crate) fn name(self, structs: &[KStructOutline]) -> &str {
        &structs[self.id].name
    }

    pub(crate) fn ty(self, structs: &[KStructOutline]) -> &KTy {
        &structs[self.id].ty
    }

    pub(crate) fn tag_ty<'a>(
        self,
        structs: &[KStructOutline],
        enums: &'a [KEnumOutline],
    ) -> &'a KTy {
        match structs[self.id].ty {
            KTy::Enum(k_enum) => k_enum.tag_ty(enums),
            _ => &KTy::Unit,
        }
    }

    pub(crate) fn fields(self, structs: &[KStructOutline]) -> &[KField] {
        &structs[self.id].fields
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct KStructOutline {
    pub(crate) name: String,
    /// 構造体の型、またはレコードバリアントが所属する enum の型
    pub(crate) ty: KTy,
    pub(crate) fields: Vec<KField>,
    pub(crate) location: Location,
}

impl KStructOutline {
    pub(crate) fn keys(structs: &[KStructOutline]) -> impl Iterator<Item = KStruct> {
        (0..structs.len()).map(KStruct::new)
    }

    pub(crate) fn iter(
        structs: &[KStructOutline],
    ) -> impl Iterator<Item = (KStruct, &KStructOutline)> {
        structs
            .iter()
            .enumerate()
            .map(|(i, struct_data)| (KStruct::new(i), struct_data))
    }
}
