use super::{k_extern_fn::KExternFnArena, k_fn::KFnArena, KOutlines};

#[derive(Debug)]
pub(crate) struct KRoot {
    pub(crate) outlines: KOutlines,
    pub(crate) fns: KFnArena,
    pub(crate) extern_fns: KExternFnArena,
}
