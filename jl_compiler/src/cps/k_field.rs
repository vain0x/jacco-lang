use super::KTy;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

/// `foo.field` の `.field` のような形で出現している、
/// 式の型に依存するいずれかの構造体のフィールドを表す。
#[derive(Clone, Debug)]
pub(crate) struct KFieldTag {
    pub(crate) name: String,
    pub(crate) loc: Loc,
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

    pub(crate) fn loc(self, fields: &KFieldArena) -> Loc {
        fields[self].loc
    }
}

#[derive(Debug)]
pub(crate) struct KFieldOutline {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    pub(crate) loc: Loc,
}
