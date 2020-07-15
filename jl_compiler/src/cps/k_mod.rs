use super::{
    k_const::KConstArena, k_enum::KEnumArena, k_extern_fn::KExternFnOutlineArena,
    k_fn::KFnOutlineArena, k_static_var::KStaticVarArena, k_struct::KStructArena, KExternFnArena,
    KFieldArena, KFnArena,
};
use crate::utils::{VecArena, VecArenaId};

pub(crate) struct KModTag;

#[allow(unused)]
pub(crate) type KMod = VecArenaId<KModTag>;

#[allow(unused)]
pub(crate) type KModOutlines = VecArena<KModTag, KModOutline>;

#[allow(unused)]
pub(crate) type KModArena = VecArena<KModTag, KModData>;

#[derive(Debug, Default)]
pub(crate) struct KModOutline {
    pub(crate) consts: KConstArena,
    pub(crate) static_vars: KStaticVarArena,
    pub(crate) fns: KFnOutlineArena,
    pub(crate) extern_fns: KExternFnOutlineArena,
    pub(crate) enums: KEnumArena,
    pub(crate) structs: KStructArena,
    pub(crate) fields: KFieldArena,
}

#[derive(Debug, Default)]
pub(crate) struct KModData {
    pub(crate) fns: KFnArena,
    pub(crate) extern_fns: KExternFnArena,
}
