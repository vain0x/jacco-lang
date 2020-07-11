use super::{
    k_const::KConstArena, k_static_var::KStaticVarArena, k_struct::KStructArena, KEnumOutline,
    KExternFnOutline, KFieldOutline, KFnOutline,
};

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: KConstArena,
    pub(crate) static_vars: KStaticVarArena,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) enums: Vec<KEnumOutline>,
    pub(crate) structs: KStructArena,
    pub(crate) fields: Vec<KFieldOutline>,
}
