#![allow(unused)]

use crate::{
    front::NName,
    utils::{VecArena, VecArenaId},
};

pub(crate) struct KAliasTag;

pub(crate) type KAlias = VecArenaId<KAliasTag>;

pub(crate) type KAliasArena = VecArena<KAliasTag, KAliasOutline>;

pub(crate) struct KAliasOutline {
    name: String,
    referent_opt: Option<NName>,
}
