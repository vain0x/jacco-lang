use super::{k_extern_fn::KExternFnArena,  KFnData, KOutlines};

#[derive(Debug)]
pub(crate) struct KRoot {
    pub(crate) outlines: KOutlines,
    pub(crate) fns: Vec<KFnData>,
    pub(crate) extern_fns: KExternFnArena,
}
