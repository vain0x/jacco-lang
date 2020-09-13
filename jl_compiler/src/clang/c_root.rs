use super::CStmt;
use crate::utils::*;

pub(crate) struct CRoot<'a> {
    pub(crate) decls: BumpVec<'a, CStmt>,
}
