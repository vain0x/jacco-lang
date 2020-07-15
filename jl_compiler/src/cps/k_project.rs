#![allow(unused)]

use super::{KModArena, KModOutlines};
use crate::utils::{VecArena, VecArenaId};

pub(crate) struct KProjectTag;

pub(crate) type KProject = VecArenaId<KProjectTag>;

pub(crate) type KProjectArena = VecArena<KProjectTag, KProjectData>;

pub(crate) struct KProjectData {
    pub(crate) name: String,
    pub(crate) mod_outlines: KModOutlines,
    pub(crate) mods: KModArena,
}
