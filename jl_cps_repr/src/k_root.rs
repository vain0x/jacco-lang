use super::{KExternFnData, KFnData, KOutlines};

#[derive(Debug)]
pub struct KRoot {
    pub outlines: KOutlines,
    pub fns: Vec<KFnData>,
    pub extern_fns: Vec<KExternFnData>,
}
