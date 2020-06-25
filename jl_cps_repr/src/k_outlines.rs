use super::{
    k_struct::KStructOutline, KConstData, KExternFnOutline, KFieldOutline, KFnOutline,
    KStaticVarData,
};

#[derive(Debug, Default)]
pub struct KOutlines {
    pub consts: Vec<KConstData>,
    pub static_vars: Vec<KStaticVarData>,
    pub fns: Vec<KFnOutline>,
    pub extern_fns: Vec<KExternFnOutline>,
    pub structs: Vec<KStructOutline>,
    pub fields: Vec<KFieldOutline>,
}
