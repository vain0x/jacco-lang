use super::{CBlock, CExpr, CTy};

pub(crate) enum CStorageModifier {
    Static,
}

pub(crate) enum CStmt {
    Comment(String),
    Expr(CExpr),
    Block(CBlock),
    Label {
        label: String,
    },
    Goto {
        label: String,
    },
    Return(Option<CExpr>),
    If {
        cond: CExpr,
        body: Box<CStmt>,
        alt: Box<CStmt>,
    },
    VarDecl {
        storage_modifier_opt: Option<CStorageModifier>,
        name: String,
        ty: CTy,
        init_opt: Option<CExpr>,
    },
    FnDecl {
        name: String,
        params: Vec<(String, CTy)>,
        result_ty: CTy,
        body: CBlock,
    },
    ExternFnDecl {
        name: String,
        params: Vec<(String, CTy)>,
        result_ty: CTy,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, CTy)>,
    },
}
