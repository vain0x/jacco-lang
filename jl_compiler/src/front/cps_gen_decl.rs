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

    fn convert_let_decl(&mut self, _decl_id: ADeclId, decl: &AFieldLikeDecl, loc: Loc) {
        let (init_term, ty) = self.evaluate_rval(decl.value_opt, decl.ty_opt, loc);

        let name = match decl.name_opt {
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

    fn convert_const_decl(&mut self, k_const: KConst, decl: &AFieldLikeDecl, loc: Loc) {
        let (node, term) = {
            let mut nodes = take(&mut self.nodes);
            let (term, _ty) =
                self.do_out_fn(|xx| xx.convert_expr_opt(decl.value_opt, TyExpect::Todo, loc));
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

    fn convert_static_decl(&mut self, static_var: KStaticVar, decl: &AFieldLikeDecl, loc: Loc) {
        let (node, term) = {
            let mut nodes = take(&mut self.nodes);
            let (term, _ty) =
                self.do_out_fn(|xx| xx.convert_expr_opt(decl.value_opt, TyExpect::Todo, loc));
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

    fn convert_fn_decl(&mut self, k_fn: KFn, fn_decl: &AFnLikeDecl, loc: Loc) {
        let mut local_vars = KLocalVarArena::new();

        let params = Self::convert_param_decls(
            &fn_decl.params,
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

            log::trace!(
                "fn result_ty={}",
                result_ty.display(&xx.ty_env, xx.mod_outline)
            );
            let (term, _ty) =
                xx.convert_expr_opt(fn_decl.body_opt, TyExpect::Exact(&result_ty), loc);
            xx.emit_return(term, loc);
            xx.commit_label();

            #[cfg(skip)]
            for label in xx.labels.iter() {
                log::trace!(
                    "label {}({:#?}) size={} = {:#?}",
                    &label.name,
                    DebugWith::new(&label.params, &xx.local_vars),
                    label.body.len(),
                    DebugWith::new(&label.body, &local_context(xx))
                );
            }

            let local_vars = take(&mut xx.local_vars);
            let labels =
                KLabelArena::from_iter(take(&mut xx.labels).into_vec().into_iter().map(|label| {
                    let name = label.name;
                    let params = label.params;
                    let body = fold_nodes(label.body);
                    KLabelData { name, params, body }
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

    fn convert_extern_fn_decl(&mut self, extern_fn: KExternFn, extern_fn_decl: &AFnLikeDecl) {
        let mut local_vars = KLocalVarArena::new();

        let params = Self::convert_param_decls(
            &extern_fn_decl.params,
            extern_fn.param_tys(&self.mod_outline.extern_fns),
            self.doc,
            self.ast,
            &mut local_vars,
            self.name_symbols,
        );

        *extern_fn.of_mut(&mut self.mod_data.extern_fns) = KExternFnData { params, local_vars };
    }

    fn convert_const_enum_decl(&mut self, const_enum: KConstEnum, decl: &AEnumDecl, loc: Loc) {
        for (decl, k_const) in decl
            .variants
            .iter()
            .zip(const_enum.variants(&self.mod_outline.const_enums).iter())
        {
            let decl = decl.as_const().unwrap();
            self.convert_const_decl(k_const, decl, loc);
        }
    }

    fn convert_struct_enum_decl(&mut self, _enum: KStructEnum, _decl: &AEnumDecl, _loc: Loc) {
        // pass
    }

    fn do_convert_decl(
        &mut self,
        decl_id: ADeclId,
        decl: &ADecl,
        ty_expect: TyExpect,
        term_opt: &mut Option<AfterRval>,
    ) {
        let symbol_opt = decl
            .name_opt()
            .and_then(|name| self.name_symbols.get(&name).cloned());
        let loc = Loc::new(self.doc, PLoc::Decl(decl_id));

        match decl {
            ADecl::Attr => {}
            ADecl::Expr(expr) => {
                *term_opt = Some(self.convert_expr(*expr, ty_expect));
            }
            ADecl::Let(decl) => {
                assert_eq!(symbol_opt, None);
                self.convert_let_decl(decl_id, decl, loc);
            }
            ADecl::Const(decl) => {
                let k_const = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::Const(it))) => it,
                    _ => return,
                };
                self.convert_const_decl(k_const, decl, loc)
            }
            ADecl::Static(decl) => {
                let static_var = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::StaticVar(it))) => it,
                    _ => return,
                };
                self.convert_static_decl(static_var, decl, loc)
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::Fn(it))) => it,
                    _ => return,
                };
                self.convert_fn_decl(k_fn, fn_decl, loc);
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = match symbol_opt {
                    Some(NameSymbol::ModSymbol(KModSymbol::ExternFn(it))) => it,
                    _ => return,
                };
                self.convert_extern_fn_decl(extern_fn, extern_fn_decl);
            }
            ADecl::Enum(enum_decl) => match symbol_opt {
                Some(NameSymbol::ModSymbol(KModSymbol::ConstEnum(const_enum))) => {
                    self.convert_const_enum_decl(const_enum, enum_decl, loc)
                }
                Some(NameSymbol::ModSymbol(KModSymbol::StructEnum(struct_enum))) => {
                    self.convert_struct_enum_decl(struct_enum, enum_decl, loc)
                }
                _ => return,
            },
            ADecl::Struct(_) | ADecl::Use(_) => {}
        }
    }

    pub(crate) fn convert_decls(
        &mut self,
        decls: ADeclIds,
        ty_expect: TyExpect,
    ) -> Option<AfterRval> {
        let mut last_opt = None;
        for (decl_id, decl) in decls.enumerate(self.ast.decls()) {
            // 最後の式文以外は unit
            let ty_expect = match decl {
                ADecl::Expr(..) if decls.is_last(decl_id) => ty_expect,
                _ => TyExpect::unit(),
            };

            self.do_convert_decl(decl_id, decl, ty_expect, &mut last_opt);
        }
        last_opt
    }
}
