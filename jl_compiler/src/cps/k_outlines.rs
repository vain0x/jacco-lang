use super::{
    k_const::KConstArena, k_enum::KEnumArena, k_static_var::KStaticVarArena,
    k_struct::KStructArena, KExternFnOutline, KFieldArena, KFnOutline,
};

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: KConstArena,
    pub(crate) static_vars: KStaticVarArena,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) enums: KEnumArena,
    pub(crate) structs: KStructArena,
    pub(crate) fields: KFieldArena,
}
