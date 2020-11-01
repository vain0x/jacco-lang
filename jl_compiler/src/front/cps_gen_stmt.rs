use crate::{
    cps::*,
    front::{cps_gen::*, name_resolution::*},
    parse::*,
    source::{Doc, Loc},
};
use std::mem::{swap, take};

impl<'a> Xx<'a> {
    /// 式を右辺値として評価する。型注釈があれば ty_opt に渡される。
    fn evaluate_rval(
        &mut self,
        expr_opt: Option<AExprId>,
        expected_ty_opt: Option<ATyId>,
        loc: Loc,
    ) -> (KTerm, KTy2) {
        let (term, _ty) = self.convert_expr_opt(expr_opt, TyExpect::Todo, loc);
        let expected_ty = self
            .ty_resolver()
            .convert_ty_opt(expected_ty_opt)
            .to_ty2_poly(self.mod_outline);
        (term, expected_ty)
    }

    fn convert_let_stmt(&mut self, _stmt_id: AStmtId, stmt: &AFieldLikeDecl, loc: Loc) {
        let (init_term, ty) = self.evaluate_rval(stmt.value_opt, stmt.ty_opt, loc);

        let name = match stmt.name_opt {
            Some(it) => it,
            None => {
                // 名前がなくても、型検査のため、一時変数に束縛する必要がある。
                let local_var = self
                    .local_vars
                    .alloc(KLocalVarData::new("_".to_string(), loc).with_ty(ty));
                let var_term = KVarTerm {
                    local_var,
                    cause: KVarTermCause::Loc(loc),
                };
                self.nodes
                    .push(new_let_node(init_term, var_term, new_cont(), loc));
                return;
            }
        };

        let local_var = self.local_vars.alloc(
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
        self.nodes
            .push(new_let_node(init_term, var_term, new_cont(), loc));
        self.name_symbols
            .insert(name, NameSymbol::LocalVar(local_var));
    }

    fn convert_const_stmt(&mut self, k_const: KConst, stmt: &AFieldLikeDecl, loc: Loc) {
        let (node, term) = {
            let mut nodes = take(&mut self.nodes);
            let (term, _ty) =
                self.do_out_fn(|xx| xx.convert_expr_opt(stmt.value_opt, TyExpect::Todo, loc));
            swap(&mut self.nodes, &mut nodes);

            (fold_nodes(nodes), term)
        };

        #[cfg(skip)]
        log::trace!(
            "{} init = {:?}",
            k_const.of(&self.mod_outline.consts).name,
            DebugWith::new(&term, &local_context(xx)),
        );
        *k_const.of_mut(&mut self.mod_data.consts) = KConstInit {
            init_opt: Some((node, term)),
        };
    }

    fn convert_static_stmt(&mut self, static_var: KStaticVar, stmt: &AFieldLikeDecl, loc: Loc) {
        let (node, term) = {
            let mut nodes = take(&mut self.nodes);
            let (term, _ty) =
                self.do_out_fn(|xx| xx.convert_expr_opt(stmt.value_opt, TyExpect::Todo, loc));
            swap(&mut self.nodes, &mut nodes);

            (fold_nodes(nodes), term)
        };

        *static_var.of_mut(&mut self.mod_data.static_vars) = KStaticVarInit {
            init_opt: Some((node, term)),
        };
    }

    fn convert_param_decls(
        param_decls: &[AParamDecl],
        param_tys: &[KTy],
        doc: Doc,
        ast: &ATree,
        local_vars: &mut KLocalVarArena,
        name_symbols: &mut NameSymbols,
    ) -> Vec<KVarTerm> {
        assert_eq!(param_decls.len(), param_tys.len());

        param_decls
            .iter()
            .zip(param_tys)
            .map(|(param_decl, _param_ty)| {
                let loc = param_decl.name.loc().to_loc(doc);
                let name = param_decl.name.of(ast.names()).text.to_string();
                let local_var = local_vars.alloc(KLocalVarData::new(name.to_string(), loc));
                name_symbols.insert(param_decl.name, NameSymbol::LocalVar(local_var));
                KVarTerm {
                    local_var,
                    cause: KVarTermCause::NameDef(doc, param_decl.name),
                }
            })
            .collect()
    }

    fn convert_fn_stmt(&mut self, k_fn: KFn, stmt: &AFnLikeStmt, loc: Loc) {
        let mut local_vars = KLocalVarArena::new();

        let params = Self::convert_param_decls(
            &stmt.params,
            k_fn.param_tys(&self.mod_outline.fns),
            self.doc,
            self.ast,
            &mut local_vars,
            self.name_symbols,
        );
        let result_ty = k_fn
            .of(&self.mod_outline.fns)
            .result_ty
            .to_ty2_poly(self.mod_outline);

        let fn_data = self.do_out_fn(|xx| {
            xx.fn_opt = Some(k_fn);
            xx.local_vars = local_vars;

            // 関数の本体を格納しておくラベル
            xx.label = xx.labels.alloc(KLabelConstruction::default());

            #[cfg(skip)]
            log::trace!(
                "fn result_ty={}",
                result_ty.display(&xx.ty_env, xx.mod_outline)
            );
            let (term, _ty) = xx.convert_expr_opt(stmt.body_opt, TyExpect::Exact(result_ty), loc);
            xx.emit_return(term, loc);
            xx.commit_label();

            let local_vars = take(&mut xx.local_vars);
            let labels =
                KLabelArena::from_iter(take(&mut xx.labels).into_vec().into_iter().map(|label| {
                    let name = label.name;
                    let parent_opt = label.parent_opt;
                    let params = label.params;
                    let body = fold_nodes(label.body);
                    KLabelData {
                        name,
                        parent_opt,
                        params,
                        body,
                    }
                }));
            let ty_env = take(&mut xx.ty_env);

            KFnData::new(params, local_vars, labels, ty_env)
        });

        *k_fn.of_mut(&mut self.mod_data.fns) = fn_data;
    }

    fn emit_return(&mut self, term: KTerm, loc: Loc) {
        let k_fn = self.fn_opt.unwrap();
        self.nodes.push(new_return_tail(k_fn, term, loc));
    }

    fn convert_extern_fn_stmt(&mut self, extern_fn: KExternFn, stmt: &AFnLikeStmt) {
        let mut local_vars = KLocalVarArena::new();

        let params = Self::convert_param_decls(
            &stmt.params,
            extern_fn.param_tys(&self.mod_outline.extern_fns),
            self.doc,
            self.ast,
            &mut local_vars,
            self.name_symbols,
        );

        *extern_fn.of_mut(&mut self.mod_data.extern_fns) = KExternFnData { params, local_vars };
    }

    fn convert_const_enum_stmt(&mut self, const_enum: KConstEnum, stmt: &AEnumStmt, loc: Loc) {
        for (variant_decl, k_const) in stmt
            .variants
            .iter()
            .zip(const_enum.variants(&self.mod_outline.const_enums).iter())
        {
            let decl = variant_decl.as_const().unwrap();
            self.convert_const_stmt(k_const, decl, loc);
        }
    }

    fn convert_struct_enum_stmt(&mut self, _enum: KStructEnum, _stmt: &AEnumStmt, _loc: Loc) {
        // pass
    }

    fn do_convert_stmt(
        &mut self,
        stmt_id: AStmtId,
        stmt: &AStmt,
        ty_expect: TyExpect,
        term_opt: &mut Option<AfterRval>,
    ) {
        let symbol_opt = stmt
            .name_opt()
            .and_then(|name| self.name_symbols.get(&name).cloned());
        let loc = Loc::new(self.doc, PLoc::Stmt(stmt_id));

        match stmt {
            AStmt::Attr => {}
            AStmt::Expr(expr) => {
                *term_opt = Some(self.convert_expr(*expr, ty_expect));
            }
            AStmt::Let(stmt) => {
                assert_eq!(symbol_opt, None);
                self.convert_let_stmt(stmt_id, stmt, loc);
            }
            AStmt::Const(stmt) => {
                let k_const = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::Const(it))) => it,
                    _ => return,
                };
                self.convert_const_stmt(k_const, stmt, loc)
            }
            AStmt::Static(stmt) => {
                let static_var = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::StaticVar(it))) => it,
                    _ => return,
                };
                self.convert_static_stmt(static_var, stmt, loc)
            }
            AStmt::Fn(stmt) => {
                let k_fn = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::Fn(it))) => it,
                    _ => return,
                };
                self.convert_fn_stmt(k_fn, stmt, loc);
            }
            AStmt::ExternFn(stmt) => {
                let extern_fn = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::ExternFn(it))) => it,
                    _ => return,
                };
                self.convert_extern_fn_stmt(extern_fn, stmt);
            }
            AStmt::Enum(stmt) => match symbol_opt {
                Some(NameSymbol::ModSymbol(KModSymbol::ConstEnum(const_enum))) => {
                    self.convert_const_enum_stmt(const_enum, stmt, loc)
                }
                Some(NameSymbol::ModSymbol(KModSymbol::StructEnum(struct_enum))) => {
                    self.convert_struct_enum_stmt(struct_enum, stmt, loc)
                }
                _ => return,
            },
            AStmt::Struct(_) | AStmt::Use(_) => {}
        }
    }

    pub(crate) fn convert_stmts(
        &mut self,
        stmts: AStmtIds,
        ty_expect: TyExpect,
    ) -> Option<AfterRval> {
        let mut last_opt = None;
        for (stmt_id, stmt) in stmts.enumerate(self.ast.stmts()) {
            // 最後の式文
            match stmt {
                AStmt::Expr(..) if stmts.is_last(stmt_id) => {
                    self.do_convert_stmt(stmt_id, stmt, ty_expect, &mut last_opt);
                    break;
                }
                _ => {}
            }

            self.do_convert_stmt(stmt_id, stmt, TyExpect::unit(), &mut last_opt);
        }
        last_opt
    }
}
