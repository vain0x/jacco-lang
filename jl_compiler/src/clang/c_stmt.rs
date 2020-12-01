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
    Switch {
        cond: CExpr,
        cases: Vec<(CExpr, CBlock)>,
        default_opt: Option<CBlock>,
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
        body_opt: Option<CBlock>,
    },
    ExternFnDecl {
        name: String,
        params: Vec<(String, CTy)>,
        result_ty: CTy,
    },
    #[allow(unused)]
    EnumDecl {
        name: String,
        variants: Vec<(String, Option<CExpr>)>,
    },
    StructDecl {
        name: String,
        fields: Vec<(String, CTy)>,

        /// anonymous union
        union_opt: Option<Vec<(String, CTy)>>,
    },
    TypeDefFnPtrDecl {
        name: String,
        param_tys: Vec<CTy>,
        result_ty: CTy,
    },
}
