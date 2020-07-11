use super::KTy;
use crate::{
    token::Location,
    utils::{VecArena, VecArenaId},
};

/// `foo.field` の `.field` のような形で出現している、
/// 式の型に依存するいずれかの構造体のフィールドを表す。
#[derive(Clone, Debug)]
pub(crate) struct KFieldTag {
    pub(crate) name: String,
    pub(crate) location: Location,
}

// とりあえず KFieldTag を使いまわしてる。
pub(crate) type KField = VecArenaId<KFieldTag>;

pub(crate) type KFieldArena = VecArena<KFieldTag, KFieldOutline>;

impl KField {
    pub(crate) fn name(self, fields: &KFieldArena) -> &str {
        &fields[self].name
    }

    pub(crate) fn ty(self, fields: &KFieldArena) -> &KTy {
        &fields[self].ty
    }

    pub(crate) fn location(self, fields: &KFieldArena) -> Location {
        fields[self].location
    }
}

#[derive(Debug)]
pub(crate) struct KFieldOutline {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) location: Location,
}
