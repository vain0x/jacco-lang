use super::{
    k_struct::KStructOutline, KConstData, KExternFnOutline, KFieldOutline, KFnOutline,
    KStaticVarData,
};

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: Vec<KConstData>,
    pub(crate) static_vars: Vec<KStaticVarData>,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}
