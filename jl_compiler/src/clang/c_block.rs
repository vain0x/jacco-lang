use super::CStmt;

pub(crate) struct CBlock {
    pub(crate) stmts: Vec<CStmt>,
}
