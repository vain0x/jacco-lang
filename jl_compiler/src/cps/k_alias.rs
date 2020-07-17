#![allow(unused)]

use super::KProjectSymbol;
use crate::{
    front::NName,
    token::Location,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KAliasTag;

pub(crate) type KAlias = VecArenaId<KAliasTag>;

pub(crate) type KAliasArena = VecArena<KAliasTag, KAliasOutline>;

#[derive(Debug)]
pub(crate) struct KAliasOutline {
    name: String,
    path: Vec<String>,
    location: Location,
    referent_opt: Option<KProjectSymbol>,
}

impl KAliasOutline {
    pub(crate) fn new(name: String, path: Vec<String>, location: Location) -> KAliasOutline {
        KAliasOutline {
            name,
            path,
            location,
            referent_opt: None,
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn path(&self) -> &[String] {
        &self.path
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn referent(&self) -> Option<KProjectSymbol> {
        self.referent_opt
    }

    pub(crate) fn bind(&mut self, referent: KProjectSymbol) {
        let old_referent = self.referent_opt.replace(referent);
        assert_eq!(old_referent, None);
    }
}
