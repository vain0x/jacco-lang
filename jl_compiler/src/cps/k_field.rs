use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

/// `foo.field` の `.field` のような形で出現している、
/// 式の型に依存するいずれかの構造体のフィールドを表す。
#[derive(Clone)]
pub(crate) struct KFieldTag {
    pub(crate) name: String,
    pub(crate) ty: KTy2,

    /// 型推論で埋める。
    pub(crate) field_opt: Option<KField>,

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
}

pub(crate) struct KFieldOutline {
    pub(crate) name: String,
    pub(crate) ty: KTy,
    #[allow(unused)]
    pub(crate) loc: Loc,
}
