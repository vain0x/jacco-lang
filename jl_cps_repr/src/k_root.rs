use super::{KExternFnData, KFnData, KOutlines};

#[derive(Debug)]
pub(crate) struct KRoot {
    pub(crate) outlines: KOutlines,
    pub(crate) fns: Vec<KFnData>,
    pub(crate) extern_fns: Vec<KExternFnData>,
}
