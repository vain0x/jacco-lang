use super::{KProjectConstEnum, KProjectStructEnum, KProjectSymbol, KTy2};
use crate::{
    source::Loc,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KAliasTag;

pub(crate) type KAlias = VecArenaId<KAliasTag>;

pub(crate) type KAliasArena = VecArena<KAliasTag, KAliasOutline>;

#[derive(Clone, Debug)]
pub(crate) struct KAliasOutline {
    name: String,
    path: Vec<String>,
    loc: Loc,
    referent_opt: Option<KProjectSymbol>,
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

    pub(crate) fn referent(&self) -> Option<KProjectSymbol> {
        self.referent_opt
    }

    pub(crate) fn referent_as_ty(&self) -> Option<KTy2> {
        let ty = match self.referent() {
            Some(KProjectSymbol::ConstEnum(KProjectConstEnum(k_mod, const_enum))) => {
                KTy2::ConstEnum(k_mod, const_enum)
            }
            Some(KProjectSymbol::StructEnum(KProjectStructEnum(k_mod, struct_enum))) => {
                KTy2::StructEnum(k_mod, struct_enum)
            }
            _ => return None,
        };
        Some(ty)
    }

    pub(crate) fn bind(&mut self, referent: KProjectSymbol) {
        let old_referent = self.referent_opt.replace(referent);
        assert_eq!(old_referent, None);
    }
}
