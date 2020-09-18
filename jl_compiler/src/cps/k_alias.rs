use super::*;
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KAliasTag;

pub(crate) type KAlias = VecArenaId<KAliasTag>;

pub(crate) type KAliasArena = VecArena<KAliasTag, KAliasOutline>;

#[derive(Clone)]
pub(crate) struct KAliasOutline {
    name: String,
    path: Vec<String>,
    loc: Loc,
    referent_opt: Option<KModSymbol>,
}

impl KAliasOutline {
    pub(crate) fn new(name: String, path: Vec<String>, loc: Loc) -> KAliasOutline {
        KAliasOutline {
            name,
            path,
            loc,
            referent_opt: None,
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn path(&self) -> &[String] {
        &self.path
    }

    pub(crate) fn loc(&self) -> Loc {
        self.loc
    }

    pub(crate) fn referent(&self) -> Option<KModSymbol> {
        self.referent_opt
    }

    pub(crate) fn referent_as_ty(&self) -> Option<KTy2> {
        let ty = match self.referent() {
            Some(KModSymbol::ConstEnum(const_enum)) => KTy2::ConstEnum(const_enum),
            Some(KModSymbol::StructEnum(struct_enum)) => KTy2::StructEnum(struct_enum),
            Some(KModSymbol::Struct(k_struct)) => KTy2::Struct(k_struct),
            _ => return None,
        };
        Some(ty)
    }

    pub(crate) fn bind(&mut self, referent: KModSymbol) {
        let old_referent = self.referent_opt.replace(referent);
        assert_eq!(old_referent, None);
    }
}
