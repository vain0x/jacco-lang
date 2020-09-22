use crate::{
    cps::*,
    front::{name_resolution::*, semantic_analyzer::*},
    logs::*,
    parse::*,
    source::*,
    utils::*,
};

use super::cps_conversion::TyExpect;

struct KLabelConstruction {
    name: String,
    params: Vec<KVarTerm>,
    body: Vec<KNode>,
}

struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

// type AfterLval = KTerm;
// type AfterRval = KTerm;
// type AfterJump = KTerm;

struct CpsCode {
    label: KLabel,
    nodes: Vec<KNode>,
    local_vars: KLocalVarArena,
    labels: VecArena<KLabelTag, KLabelConstruction>,
    ty_env: KTyEnv,
    /// return のターゲットとなる関数
    fn_opt: Option<KFn>,
    /// break/continue のターゲットとなるループ
    loop_opt: Option<KLoopData>,
}

impl CpsCode {
    pub(crate) fn new(label: KLabel) -> Self {
        Self {
            label,
            nodes: Default::default(),
            local_vars: Default::default(),
            labels: Default::default(),
            ty_env: Default::default(),
            fn_opt: Default::default(),
            loop_opt: Default::default(),
        }
    }
}

pub(crate) struct CpsGenerator<'a> {
    // state:
    mod_data: &'a mut KModData,
    code: CpsCode,

    // read:
    doc: Doc,
    tokens: &'a PTokens,
    ast: &'a ATree,
    name_referents: &'a NameReferents,
    name_symbols: &'a mut NameSymbols,
    mod_outline: &'a KModOutline,
    logger: &'a DocLogger,
}

impl<'a> CpsGenerator<'a> {
    pub(crate) fn new(
        mod_data: &'a mut KModData,
        doc: Doc,
        tokens: &'a PTokens,
        ast: &'a ATree,
        name_referents: &'a NameReferents,
        name_symbols: &'a mut NameSymbols,
        mod_outline: &'a KModOutline,
        logger: &'a DocLogger,
    ) -> Self {
        let mut labels = VecArena::new();

        let toplevel = labels.alloc(KLabelConstruction {
            name: String::new(),
            params: vec![],
            body: vec![],
        });

        Self {
            mod_data,
            code: CpsCode::new(toplevel),
            doc,
            tokens,
            ast,
            name_referents,
            name_symbols,
            mod_outline,
            logger,
        }
    }

    fn get_symbol(&self, name_opt: Option<ANameId>) -> Option<NameSymbol> {
        name_opt.and_then(|name| self.name_symbols.get(&name).cloned())
    }

    pub(crate) fn after_ty_infer_missing(&mut self) -> AfterTy {
        KTy::Unresolved {
            cause: KTyCause::Miss,
        }
    }

    pub(crate) fn after_rval_expr_missing(&mut self) -> AfterRval {
        None
    }

    pub(crate) fn after_let_decl(
        &mut self,
        decl_id: ADeclId,
        decl: &AFieldLikeDecl,
        init: AfterRval,
        ty: AfterTy,
    ) -> AfterDecl {
        assert!(self.get_symbol(decl.name_opt).is_none());

        if let Some(name) = decl.name_opt {
            let local_var = self.code.local_vars.alloc(
                KLocalVarData::new(
                    name.of(self.ast.names()).text().to_string(),
                    name.loc().to_loc(self.doc),
                )
                .with_ty(ty),
            );
            let var_term = KVarTerm {
                local_var,
                cause: KVarTermCause::NameDef(self.doc, name),
            };
            let loc = decl_id.loc().to_loc(self.doc);
            self.code
                .nodes
                .push(new_let_node(init, var_term, new_cont(), loc));
            self.name_symbols
                .insert(name, NameSymbol::LocalVar(local_var));
        }

        // let name = match decl.name_opt {
        //     Some(it) => it,
        //     None => {
        //         // 名前がなくても、型検査のため、一時変数に束縛する必要がある。
        //         let local_var = xx
        //             .local_vars
        //             .alloc(KLocalVarData::new("_".to_string(), loc).with_ty(ty));
        //         let var_term = KVarTerm {
        //             local_var,
        //             cause: KVarTermCause::Loc(loc),
        //         };
        //         xx.nodes
        //             .push(new_let_node(init_term, var_term, new_cont(), loc));
        //         return;
        //     }
        // };
    }

    pub(crate) fn before_decls(&mut self, _decls: &ADeclIds) -> BeforeDecls {
        None
    }

    pub(crate) fn after_decls(&mut self, last_opt: BeforeDecls) -> AfterDecls {
        last_opt
    }
}

fn new_cont() -> KNode {
    KNode::default()
}
