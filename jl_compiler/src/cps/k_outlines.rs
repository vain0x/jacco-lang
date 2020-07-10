use super::{
    k_const::KConstArena, k_struct::KStructOutline, KEnumOutline, KExternFnOutline, KFieldOutline,
    KFnOutline, KStaticVarData,
};

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: KConstArena,
    pub(crate) static_vars: Vec<KStaticVarData>,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) enums: Vec<KEnumOutline>,
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}
