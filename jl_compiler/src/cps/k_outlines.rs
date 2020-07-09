use super::{
    k_struct::KStructOutline, KConstData, KEnumOutline, KExternFnOutline, KFieldOutline,
    KFnOutline, KStaticVarData,
};
use crate::utils::VecArena;

#[derive(Debug, Default)]
pub(crate) struct KOutlines {
    pub(crate) consts: VecArena<KConstData>,
    pub(crate) static_vars: Vec<KStaticVarData>,
    pub(crate) fns: Vec<KFnOutline>,
    pub(crate) extern_fns: Vec<KExternFnOutline>,
    pub(crate) enums: Vec<KEnumOutline>,
    pub(crate) structs: Vec<KStructOutline>,
    pub(crate) fields: Vec<KFieldOutline>,
}
