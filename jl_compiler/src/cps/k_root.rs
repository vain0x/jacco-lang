use super::{KExternFnData, KFnData};

#[derive(Clone, Debug)]
pub(crate) struct KRoot {
    pub(crate) fns: Vec<KFnData>,
    pub(crate) extern_fns: Vec<KExternFnData>,
}
