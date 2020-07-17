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
    path: Vec<String>,
    location: Location,
    referent_opt: Option<KProjectSymbol>,
}

impl KAliasOutline {
    pub(crate) fn new(path: Vec<String>, location: Location) -> KAliasOutline {
        KAliasOutline {
            path,
            location,
            referent_opt: None,
        }
    }

    pub(crate) fn name(&self) -> &str {
        match self.path.last() {
            Some(name) => name.as_str(),
            None => "<anonymous_alias>",
        }
    }

    pub(crate) fn path(&self) -> &[String] {
        &self.path
    }

    pub(crate) fn location(&self) -> Location {
        self.location
    }

    pub(crate) fn bind(&mut self, referent: KProjectSymbol) {
        let old_referent = self.referent_opt.replace(referent);
        assert_eq!(old_referent, None);
    }
}
