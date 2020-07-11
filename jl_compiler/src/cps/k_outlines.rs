use super::{
    k_const::KConstArena, k_enum::KEnumArena, k_extern_fn::KExternFnOutlineArena,
    k_static_var::KStaticVarArena, k_struct::KStructArena, KFieldArena, KFnOutline,
};

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: KConstArena,
    pub(crate) static_vars: KStaticVarArena,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: KExternFnOutlineArena,
    pub(crate) enums: KEnumArena,
    pub(crate) structs: KStructArena,
    pub(crate) fields: KFieldArena,
}
