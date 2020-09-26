use crate::{
    cps::*,
    front::{cps_gen::*, cps_gen_lit::*, name_resolution::*},
    logs::*,
    parse::*,
    source::*,
};
use std::{collections::HashMap, collections::HashSet, iter::once, mem::take};

impl<'a> Xx<'a> {
    fn new_break_label(&mut self, hint: &str, result: KVarTerm) -> BreakLabel {
        let label = self.labels.alloc(KLabelConstruction {
            name: hint.to_string(),
            params: vec![result],
            body: vec![],
        });
        BreakLabel { label, result }
    }

    fn new_continue_label(&mut self, hint: &str) -> ContinueLabel {
        let label = self.labels.alloc(KLabelConstruction {
            name: hint.to_string(),
            params: vec![],
            body: vec![],
        });
        ContinueLabel { label }
    }

    /// 現在のラベルの変換を一時中断する。
    ///
    /// 設計: これを直接使って変換を記述すると、コードの構造がとても分かりにくくなる。
    ///         代わりに、構造化された API である do_with_break/do_with_continue を使う。
    pub(crate) fn commit_label(&mut self) {
        let nodes = take(&mut self.nodes);
        self.label.of_mut(&mut self.labels).body.extend(nodes);
    }

    /// break のような進行方向へのジャンプを含む変換を行う。
    ///
    /// loop からの break だけでなく、if/match から下に抜けるときのジャンプにも使える。
    /// 変換の評価値は `result` に束縛すること。
    fn do_with_break(
        &mut self,
        break_label: BreakLabel,
        f: impl FnOnce(&mut Xx, KLabel),
    ) -> AfterRval {
        f(self, break_label.label);
        self.commit_label();

        self.label = break_label.label;
        KTerm::Name(break_label.result)
    }

    /// continue のような進行方向と逆行するジャンプを含む変換を行う。
    fn do_with_continue(
        &mut self,
        continue_label: ContinueLabel,
        loc: Loc,
        f: impl FnOnce(&mut Xx, KLabel),
    ) {
        self.nodes
            .push(new_jump_tail(continue_label.label, vec![], loc));
        self.commit_label();

        self.label = continue_label.label;
        f(self, continue_label.label)
    }

    /// 分岐の CPS 変換を行う。
    ///
    /// 変換前後でラベルを戻す必要がある。
    fn do_in_branch(&mut self, f: impl FnOnce(&mut Xx)) {
        let label = self.label;
        self.commit_label();

        f(self);
        self.commit_label();

        self.label = label;
        assert_eq!(self.nodes.len(), 0);
    }

    /// ループの本体の変換を行う。
    ///
    /// ループの結果を束縛する一時変数 `result` と
    /// break/continue のジャンプ先のラベルが生成される。
    fn do_in_loop(
        &mut self,
        hint: &str,
        loc: Loc,
        f: impl FnOnce(&mut Xx, KVarTerm, KLabel, KLabel),
    ) -> AfterRval {
        let result = self.fresh_var(hint, loc);

        // continue → break の順で生成しないと型検査の順番がおかしくなる。
        let continue_label = self.new_continue_label("continue_");
        let break_label = self.new_break_label("next", result);

        self.do_with_break(break_label, |xx, break_label| {
            xx.do_with_continue(continue_label, loc, |xx, continue_label| {
                let parent_loop_opt = xx.loop_opt.replace(KLoopData {
                    break_label,
                    continue_label,
                });

                f(xx, result, break_label, continue_label);

                xx.loop_opt = parent_loop_opt;
            });
        })
    }

    pub(crate) fn convert_name_expr(&mut self, name: ANameId) -> AfterRval {
        let loc = name.loc().to_loc(self.doc);
        let cause = KVarTermCause::NameUse(self.doc, name);

        let value = match resolve_value_path(name, self.path_resolution_context()) {
            Some(it) => it,
            None => {
                error_unresolved_value(name.loc(), self.logger);
                return new_error_term(loc);
            }
        };

        let value = match value {
            KValueOrAlias::Value(it) => it,
            KValueOrAlias::Alias(alias) => return KTerm::Alias { alias, loc },
        };

        match value {
            KLocalValue::LocalVar(local_var) => KTerm::Name(KVarTerm { local_var, cause }),
            KLocalValue::Const(k_const) => KTerm::Const { k_const, loc },
            KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
            KLocalValue::Fn(k_fn) => KTerm::Fn {
                k_fn,
                ty: k_fn
                    .ty(&self.mod_outline.fns)
                    .to_ty2(self.mod_outline, &mut self.ty_env),
                loc,
            },
            KLocalValue::ExternFn(extern_fn) => KTerm::ExternFn { extern_fn, loc },
            KLocalValue::UnitLikeStruct(k_struct) => {
                let name = k_struct.name(&self.mod_outline.structs);
                let result = self.fresh_var(name, cause);
                emit_unit_like_struct(k_struct, result, loc, &mut self.nodes)
            }
        }
    }

    fn convert_name_lval(&mut self, name: ANameId, k_mut: KMut) -> AfterLval {
        let loc = name.loc().to_loc(self.doc);
        let cause = KVarTermCause::NameUse(self.doc, name);

        let value = match resolve_value_path(name, self.path_resolution_context()) {
            Some(KValueOrAlias::Value(it)) => it,
            Some(KValueOrAlias::Alias(alias)) => return KTerm::Alias { alias, loc },
            None => {
                error_unresolved_value(name.loc(), self.logger);
                return new_error_term(loc);
            }
        };

        let term = match value {
            KLocalValue::LocalVar(local_var) => KTerm::Name(KVarTerm { local_var, cause }),
            KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
            KLocalValue::Const(_)
            | KLocalValue::Fn(_)
            | KLocalValue::ExternFn(_)
            | KLocalValue::UnitLikeStruct(_) => {
                error_rval_used_as_lval(name.loc(), self.logger);
                return new_error_term(loc);
            }
        };

        match k_mut {
            KMut::Const => {
                let result = self.fresh_var("ref", cause);
                self.nodes.push(new_ref_node(term, result, new_cont(), loc));
                KTerm::Name(result)
            }
            KMut::Mut => {
                let result = self.fresh_var("refmut", cause);
                self.nodes
                    .push(new_ref_mut_node(term, result, new_cont(), loc));
                KTerm::Name(result)
            }
        }
    }

    fn convert_ty_app_expr(&mut self, expr: &ATyAppExpr) -> AfterRval {
        let name = expr.left;
        let loc = name.loc().to_loc(self.doc);

        // __align_of/__size_of
        if let Some(kind) = KTyProperty::from_str(name.of(self.ast.names()).text()) {
            let ty_arg = match expr.ty_args.iter().next() {
                Some(it) if expr.ty_args.len() == 1 => it,
                _ => {
                    error_ty_arg_arity(name.loc(), self.logger);
                    return new_error_term(loc);
                }
            };

            let ty = self.ty_resolver().convert_ty(ty_arg);
            return KTerm::TyProperty { kind, ty, loc };
        }

        let value = match resolve_value_path(name, self.path_resolution_context()) {
            Some(it) => it,
            None => {
                error_unresolved_value(name.loc(), self.logger);
                return new_error_term(loc);
            }
        };

        match value {
            KValueOrAlias::Value(KLocalValue::Fn(k_fn)) => {
                let fn_data = k_fn.of(&self.mod_outline.fns);
                if fn_data.ty_params.is_empty() {
                    error_invalid_ty_args(name.loc(), self.logger);
                    return new_error_term(loc);
                }

                if expr.ty_args.len() != fn_data.ty_params.len() {
                    error_ty_arg_arity(name.loc(), self.logger);
                    return new_error_term(loc);
                }

                let ty_args = fn_data
                    .ty_params
                    .iter()
                    .zip(expr.ty_args.iter())
                    .map(|(ty_param, ty_arg)| {
                        let ty_param = ty_param.name.to_string();
                        let ty_arg = self
                            .ty_resolver()
                            .convert_ty(ty_arg)
                            .to_ty2(self.mod_outline, &mut self.ty_env);
                        (ty_param, ty_arg)
                    })
                    .collect::<HashMap<_, _>>();
                let ty = fn_data.ty().substitute(&self.mod_outline, &ty_args);

                KTerm::Fn { k_fn, ty, loc }
            }
            _ => {
                self.logger
                    .error(name.loc(), "ジェネリックな関数以外の型適用式は未実装です");
                new_error_term(loc)
            }
        }
    }

    fn do_convert_record_expr(&mut self, expr: &ARecordExpr, loc: Loc) -> Option<KVarTerm> {
        let k_struct = match resolve_ty_path(expr.left, self.path_resolution_context()) {
            Some(KTy2::Struct(k_struct)) => k_struct,
            _ => {
                error_expected_record_ty(PLoc::from_loc(loc), self.logger);
                return None;
            }
        };

        let mut ty_is_generic = false;
        let loc_ty_args = PLoc::TokenBehind(expr.left.of(self.ast.names()).token);
        let ty = match &k_struct.of(&self.mod_outline.structs).parent {
            KStructParent::Struct { ty_params } if !ty_params.is_empty() => {
                ty_is_generic = true;

                let mut ty_args_opt = None;
                if let Some(ty_args) = expr.ty_args_opt.as_ref() {
                    if ty_args.len() != ty_params.len() {
                        error_ty_arg_arity(loc_ty_args, self.logger);
                    } else {
                        ty_args_opt = Some(
                            ty_params
                                .iter()
                                .zip(ty_args.iter())
                                .map(|(ty_param, ty_arg)| {
                                    let ty_param = ty_param.name.to_string();
                                    let ty_arg = self
                                        .ty_resolver()
                                        .convert_ty(ty_arg)
                                        .to_ty2_poly(self.mod_outline);
                                    (ty_param, ty_arg)
                                })
                                .collect::<HashMap<_, _>>(),
                        );
                    }
                }

                let ty_args = ty_args_opt.unwrap_or_else(|| {
                    ty_params
                        .iter()
                        .map(|ty_param| {
                            let meta_ty = self.ty_env.alloc(KMetaTyData::new_fresh(ty_param.loc));
                            (ty_param.name.to_string(), KTy2::Meta(meta_ty))
                        })
                        .collect::<HashMap<_, _>>()
                });

                KTy2::App { k_struct, ty_args }
            }
            _ => KTy2::Struct(k_struct),
        };

        if expr.ty_args_opt.is_some() && !ty_is_generic {
            error_invalid_ty_args(loc_ty_args, self.logger);
        }

        let fields = &k_struct.of(&self.mod_outline.structs).fields;
        let perm = match calculate_field_ordering(
            &expr.fields,
            fields,
            &self.mod_outline.fields,
            |field_expr| &field_expr.field_name.of(self.ast.names()).text,
        ) {
            Ok(it) => it,
            Err(errors) => {
                report_record_expr_errors(fields, &errors, loc, self.mod_outline, self.logger);
                return None;
            }
        };

        let mut args = vec![KTerm::Unit { loc }; fields.len()];
        for (i, field_expr) in expr.fields.iter().enumerate() {
            args[perm[i]] = self.convert_expr_opt(field_expr.value_opt, TyExpect::Todo, loc);
        }

        let result = self.fresh_var(&k_struct.of(&self.mod_outline.structs).name, loc);
        self.nodes
            .push(new_record_node(ty, args, result, new_cont(), loc));
        Some(result)
    }

    fn convert_record_expr(&mut self, expr: &ARecordExpr, loc: Loc) -> AfterRval {
        match self.do_convert_record_expr(expr, loc) {
            Some(result) => KTerm::Name(result),
            None => new_error_term(loc),
        }
    }

    // `&A { .. }`
    fn convert_record_lval(&mut self, expr: &ARecordExpr, loc: Loc) -> AfterLval {
        let arg = match self.do_convert_record_expr(expr, loc) {
            Some(it) => it,
            None => return new_error_term(loc),
        };

        let result = self.fresh_var("ref", loc);
        self.nodes
            .push(new_ref_node(KTerm::Name(arg), result, new_cont(), loc));
        KTerm::Name(result)
    }

    // `x.field` ==> `*(&x)->field`
    fn convert_field_expr(&mut self, expr: &AFieldExpr, loc: Loc) -> AfterRval {
        let result = {
            let name = match expr.field_opt {
                Some(token) => token.text(self.tokens),
                None => "_",
            };
            self.fresh_var(&name, loc)
        };

        let field_ptr = self.convert_field_lval(expr, KMut::Const, loc);
        self.nodes
            .push(new_deref_node(field_ptr, result, new_cont(), loc));
        KTerm::Name(result)
    }

    // `&x.field` ==> `&(&x)->field`
    fn convert_field_lval(&mut self, expr: &AFieldExpr, k_mut: KMut, loc: Loc) -> AfterLval {
        let (name, field_loc) = match expr.field_opt {
            Some(token) => {
                let name = token.text(self.tokens).to_string();
                let loc = Loc::new(self.doc, PLoc::Token(token));
                (name, loc)
            }
            None => ("_".to_string(), loc),
        };
        let result = self.fresh_var(&format!("{}_ptr", name), loc);

        let left = self.convert_lval(expr.left, k_mut, TyExpect::Todo);
        self.nodes.push(new_field_node(
            left,
            name,
            field_loc,
            k_mut,
            result,
            new_cont(),
            loc,
        ));
        KTerm::Name(result)
    }

    fn convert_call_expr(
        &mut self,
        call_expr: &ACallLikeExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let result = self.fresh_var("call_result", loc);
        let left = self.convert_expr(call_expr.left, TyExpect::Todo);

        let mut args = Vec::with_capacity(call_expr.args.len() + 1);
        args.push(left);
        args.extend(
            call_expr
                .args
                .iter()
                .map(|arg| self.convert_expr(arg, TyExpect::Todo)),
        );

        self.nodes
            .push(new_call_node(args, result, new_cont(), loc));
        KTerm::Name(result)
    }

    // `a[i]` ==> `*(a + i)`
    fn convert_index_expr(
        &mut self,
        expr: &ACallLikeExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let ptr = self.convert_index_lval(expr, TyExpect::Todo, loc);

        let result = self.fresh_var("index_result", loc);
        self.nodes
            .push(new_deref_node(ptr, result, new_cont(), loc));
        KTerm::Name(result)
    }

    // `&a[i]` ==> `a + i`
    fn convert_index_lval(
        &mut self,
        expr: &ACallLikeExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterLval {
        let left = self.convert_expr(expr.left, TyExpect::Todo);
        let right = match expr.args.iter().next() {
            Some(right) if expr.args.len() == 1 => self.convert_expr(right, TyExpect::Todo),
            _ => new_error_term(loc),
        };

        let result = self.fresh_var("indexed_ptr", loc);
        self.nodes
            .push(new_add_node(left, right, result, new_cont(), loc));
        KTerm::Name(result)
    }

    fn convert_cast_expr(&mut self, expr: &ACastExpr, _ty_expect: TyExpect, loc: Loc) -> AfterRval {
        let ty = self
            .ty_resolver()
            .convert_ty_opt(expr.ty_opt)
            .to_ty2_poly(self.mod_outline);
        let arg = self.convert_expr(expr.left, TyExpect::from(&ty));

        let result = self.fresh_var("cast", loc);
        self.nodes
            .push(new_cast_node(ty, arg, result, new_cont(), loc));
        KTerm::Name(result)
    }

    fn convert_unary_op_expr(
        &mut self,
        expr: &AUnaryOpExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        match expr.op {
            PUnaryOp::Deref => {
                let arg = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);
                let result = self.fresh_var("deref", loc);
                self.nodes
                    .push(new_deref_node(arg, result, new_cont(), loc));
                KTerm::Name(result)
            }
            PUnaryOp::Ref => {
                let k_mut = expr.mut_opt.unwrap_or(KMut::Const);
                self.convert_lval_opt(expr.arg_opt, k_mut, TyExpect::Todo, loc)
            }
            PUnaryOp::Minus => {
                let arg = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);
                let result = self.fresh_var("minus", loc);
                self.nodes
                    .push(new_minus_node(arg, result, new_cont(), loc));
                KTerm::Name(result)
            }
            PUnaryOp::Not => {
                let arg = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);
                let result = self.fresh_var("not", loc);
                self.nodes.push(new_not_node(arg, result, new_cont(), loc));
                KTerm::Name(result)
            }
        }
    }

    fn convert_unary_op_lval(
        &mut self,
        expr: &AUnaryOpExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterLval {
        match expr.op {
            PUnaryOp::Deref => {
                // `&*p` ==> `p`
                self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc)
            }
            PUnaryOp::Ref | PUnaryOp::Minus | PUnaryOp::Not => {
                error_rval_used_as_lval(PLoc::from_loc(loc), self.logger);
                new_error_term(loc)
            }
        }
    }

    fn do_convert_assignment_expr(
        &mut self,
        prim: KPrim,
        expr: &ABinaryOpExpr,
        loc: Loc,
    ) -> AfterRval {
        let left = self.convert_lval(expr.left, KMut::Mut, TyExpect::Todo);
        let right = self.convert_expr_opt(expr.right_opt, TyExpect::Todo, loc);

        self.nodes
            .push(new_assignment_node(prim, left, right, new_cont(), loc));
        new_unit_term(loc)
    }

    fn do_convert_basic_binary_op_expr(
        &mut self,
        prim: KPrim,
        expr: &ABinaryOpExpr,
        loc: Loc,
    ) -> AfterRval {
        let left = self.convert_expr(expr.left, TyExpect::Todo);
        let right = self.convert_expr_opt(expr.right_opt, TyExpect::Todo, loc);

        let result = self.fresh_var(&prim.hint_str(), loc);
        self.nodes.push(new_basic_binary_op_node(
            prim,
            left,
            right,
            result,
            new_cont(),
            loc,
        ));
        KTerm::Name(result)
    }

    // `p && q` ==> `if p { q } else { false }`
    fn do_convert_log_and_expr(
        &mut self,
        expr: &ABinaryOpExpr,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        self.do_convert_if_expr(
            |xx| xx.convert_expr(expr.left, TyExpect::Todo),
            |xx| xx.convert_expr_opt(expr.right_opt, TyExpect::Todo, loc),
            |_| KTerm::False { loc },
            ty_expect,
            loc,
        )
    }

    // `p || q` ==> `if p { true } else { q }`
    fn do_convert_log_or_expr(
        &mut self,
        expr: &ABinaryOpExpr,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        self.do_convert_if_expr(
            |xx| xx.convert_expr(expr.left, TyExpect::Todo),
            |_| KTerm::True { loc },
            |xx| xx.convert_expr_opt(expr.right_opt, TyExpect::Todo, loc),
            ty_expect,
            loc,
        )
    }

    fn convert_binary_op_expr(
        &mut self,
        expr: &ABinaryOpExpr,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let xx = self;
        let on_assign = |prim: KPrim, xx| Xx::do_convert_assignment_expr(xx, prim, expr, loc);
        let on_basic = |prim: KPrim, xx| Xx::do_convert_basic_binary_op_expr(xx, prim, expr, loc);
        let on_bit = on_basic;
        let on_comparison = on_basic;

        match expr.op {
            PBinaryOp::Assign => on_assign(KPrim::Assign, xx),
            PBinaryOp::AddAssign => on_assign(KPrim::AddAssign, xx),
            PBinaryOp::SubAssign => on_assign(KPrim::SubAssign, xx),
            PBinaryOp::MulAssign => on_assign(KPrim::MulAssign, xx),
            PBinaryOp::DivAssign => on_assign(KPrim::DivAssign, xx),
            PBinaryOp::ModuloAssign => on_assign(KPrim::ModuloAssign, xx),
            PBinaryOp::BitAndAssign => on_assign(KPrim::BitAndAssign, xx),
            PBinaryOp::BitOrAssign => on_assign(KPrim::BitOrAssign, xx),
            PBinaryOp::BitXorAssign => on_assign(KPrim::BitXorAssign, xx),
            PBinaryOp::LeftShiftAssign => on_assign(KPrim::LeftShiftAssign, xx),
            PBinaryOp::RightShiftAssign => on_assign(KPrim::RightShiftAssign, xx),
            PBinaryOp::Add => on_basic(KPrim::Add, xx),
            PBinaryOp::Sub => on_basic(KPrim::Sub, xx),
            PBinaryOp::Mul => on_basic(KPrim::Mul, xx),
            PBinaryOp::Div => on_basic(KPrim::Div, xx),
            PBinaryOp::Modulo => on_basic(KPrim::Modulo, xx),
            PBinaryOp::BitAnd => on_bit(KPrim::BitAnd, xx),
            PBinaryOp::BitOr => on_bit(KPrim::BitOr, xx),
            PBinaryOp::BitXor => on_bit(KPrim::BitXor, xx),
            PBinaryOp::LeftShift => on_bit(KPrim::LeftShift, xx),
            PBinaryOp::RightShift => on_bit(KPrim::RightShift, xx),
            PBinaryOp::Equal => on_comparison(KPrim::Equal, xx),
            PBinaryOp::NotEqual => on_comparison(KPrim::NotEqual, xx),
            PBinaryOp::LessThan => on_comparison(KPrim::LessThan, xx),
            PBinaryOp::LessEqual => on_comparison(KPrim::LessEqual, xx),
            PBinaryOp::GreaterThan => on_comparison(KPrim::GreaterThan, xx),
            PBinaryOp::GreaterEqual => on_comparison(KPrim::GreaterEqual, xx),
            PBinaryOp::LogAnd => xx.do_convert_log_and_expr(expr, ty_expect, loc),
            PBinaryOp::LogOr => xx.do_convert_log_or_expr(expr, ty_expect, loc),
        }
    }

    fn convert_block_expr(&mut self, decls: ADeclIds, _ty_expect: TyExpect, loc: Loc) -> AfterRval {
        self.convert_decls(decls.clone())
            .unwrap_or_else(|| KTerm::Unit { loc })
    }

    fn convert_break_expr(
        &mut self,
        expr: &AJumpExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterJump {
        let label_opt = self.loop_opt.as_ref().map(|data| data.break_label);
        let label = match label_opt {
            Some(it) => it,
            None => {
                error_break_out_of_loop(PLoc::from_loc(loc), self.logger);
                return new_error_term(loc);
            }
        };

        let arg = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);
        self.nodes
            .push(new_jump_node(label, vec![arg], new_cont(), loc));
        new_never_term(loc)
    }

    fn convert_continue_expr(&mut self, _ty_expect: TyExpect, loc: Loc) -> AfterJump {
        let label_opt = self.loop_opt.as_ref().map(|data| data.continue_label);
        let label = match label_opt {
            Some(it) => it,
            None => {
                error_continue_out_of_loop(PLoc::from_loc(loc), self.logger);
                return new_error_term(loc);
            }
        };

        self.nodes.push(new_jump_node(
            label,
            vec![new_unit_term(loc)],
            new_cont(),
            loc,
        ));
        new_never_term(loc)
    }

    fn convert_return_expr(
        &mut self,
        expr: &AJumpExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterJump {
        let arg = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);

        let node = match self.fn_opt {
            Some(k_fn) => new_return_node(k_fn, arg, loc),
            None => {
                error_return_out_of_fn(PLoc::from_loc(loc), self.logger);
                new_error_node(loc)
            }
        };

        self.nodes.push(node);
        new_never_term(loc)
    }

    fn do_convert_if_expr(
        &mut self,
        cond_fn: impl FnOnce(&mut Xx) -> AfterRval,
        body_fn: impl FnOnce(&mut Xx) -> AfterRval,
        alt_fn: impl FnOnce(&mut Xx) -> AfterRval,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let result = self.fresh_var("if_result", loc);
        // FIXME: next → if_next
        let next = self.new_break_label("next", result);

        self.do_with_break(next, |xx, break_label| {
            let cond = cond_fn(xx);
            xx.nodes
                .push(new_if_node(cond, new_cont(), new_cont(), loc));

            xx.do_in_branch(|xx| {
                let body = body_fn(xx);
                xx.nodes.push(new_jump_tail(break_label, once(body), loc));
            });

            xx.do_in_branch(|xx| {
                let alt = alt_fn(xx);
                xx.nodes.push(new_jump_tail(break_label, once(alt), loc));
            });
        })
    }

    fn convert_if_expr(&mut self, expr: &AIfExpr, ty_expect: TyExpect, loc: Loc) -> AfterRval {
        self.do_convert_if_expr(
            |xx| xx.convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc),
            |xx| xx.convert_expr_opt(expr.body_opt, TyExpect::Todo, loc),
            |xx| xx.convert_expr_opt(expr.alt_opt, TyExpect::Todo, loc),
            ty_expect,
            loc,
        )
    }

    fn convert_match_expr(
        &mut self,
        expr: &AMatchExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let cond = self.convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc);
        if expr.arms.is_empty() {
            error_empty_match(PLoc::from_loc(loc), self.logger);
            return new_error_term(loc);
        }

        let result = self.fresh_var("match_result", loc);
        let next = self.new_break_label("match_next", result);

        self.do_with_break(next, |xx, break_label| {
            let arms = expr
                .arms
                .iter()
                .map(|arm| {
                    let term = match xx.convert_pat_opt_as_cond(arm.pat_opt, arm.loc) {
                        Branch::Case(term) => term,
                        Branch::Default(term) => KTerm::Name(term),
                    };
                    (arm, term)
                })
                .collect::<Vec<_>>();

            let switch_node = {
                let args = once(cond.clone())
                    .chain(arms.iter().map(|(_, term)| term.clone()))
                    .collect();
                let cont_count = expr.arms.len();
                new_switch_tail(args, vec![new_cont(); cont_count], loc)
            };
            xx.nodes.push(switch_node);

            for (arm, term) in arms {
                xx.do_in_branch(|xx| {
                    xx.convert_pat_opt_as_assign(arm.pat_opt, &cond, term);
                    let body = xx.convert_expr_opt(arm.body_opt, TyExpect::Todo, loc);
                    let node = new_jump_tail(break_label, vec![body], loc);
                    xx.nodes.push(node);
                });
            }
        })
    }

    // `while cond { body }` ==> `loop { if cond { body } else { break } }`
    fn convert_while_expr(
        &mut self,
        expr: &AWhileExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let unit_term = new_unit_term(loc);

        self.do_in_loop("while_result", loc, |xx, _, break_label, continue_label| {
            let cond = xx.convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc);
            xx.nodes
                .push(new_if_node(cond, new_cont(), new_cont(), loc));

            // body
            xx.do_in_branch(|xx| {
                let node = {
                    let _term = xx.convert_expr_opt(expr.body_opt, TyExpect::Todo, loc);
                    new_jump_tail(continue_label, vec![], loc)
                };
                xx.nodes.push(node);
            });

            // alt
            xx.do_in_branch(|xx| {
                xx.nodes
                    .push(new_jump_tail(break_label, vec![unit_term.clone()], loc));
            });
        })
    }

    fn convert_loop_expr(&mut self, expr: &ALoopExpr, _ty_expect: TyExpect, loc: Loc) -> AfterRval {
        self.do_in_loop("loop_result", loc, |xx, _, _, continue_label| {
            xx.do_in_branch(|xx| {
                let node = {
                    let _term = xx.convert_expr_opt(expr.body_opt, TyExpect::Todo, loc);
                    new_jump_tail(continue_label, vec![], loc)
                };
                xx.nodes.push(node);
            });
        })
    }

    fn do_convert_expr(
        &mut self,
        expr_id: AExprId,
        expr: &AExpr,
        ty_expect: TyExpect,
    ) -> AfterRval {
        let loc = Loc::new(self.doc, PLoc::Expr(expr_id));

        match expr {
            AExpr::Unit => KTerm::Unit { loc },
            AExpr::True => KTerm::True { loc },
            AExpr::False => KTerm::False { loc },
            AExpr::Number(token) => {
                convert_number_lit(*token, ty_expect, self.tokens, self.doc, self.logger)
            }
            AExpr::Char(token) => convert_char_expr(*token, self.doc, self.tokens),
            AExpr::Str(token) => convert_str_expr(*token, self.doc, self.tokens),
            AExpr::Name(name) => self.convert_name_expr(*name),
            AExpr::TyApp(expr) => self.convert_ty_app_expr(expr),
            AExpr::Record(record_expr) => self.convert_record_expr(record_expr, loc),
            AExpr::Field(field_expr) => self.convert_field_expr(field_expr, loc),
            AExpr::Call(call_expr) => self.convert_call_expr(call_expr, ty_expect, loc),
            AExpr::Index(index_expr) => self.convert_index_expr(index_expr, ty_expect, loc),
            AExpr::Cast(cast_expr) => self.convert_cast_expr(cast_expr, ty_expect, loc),
            AExpr::UnaryOp(unary_op_expr) => {
                self.convert_unary_op_expr(unary_op_expr, ty_expect, loc)
            }
            AExpr::BinaryOp(binary_op_expr) => {
                self.convert_binary_op_expr(binary_op_expr, ty_expect, loc)
            }
            AExpr::Block(ABlockExpr { decls }) => {
                self.convert_block_expr(decls.clone(), ty_expect, loc)
            }
            AExpr::Break(break_expr) => self.convert_break_expr(break_expr, ty_expect, loc),
            AExpr::Continue => self.convert_continue_expr(ty_expect, loc),
            AExpr::Return(return_expr) => self.convert_return_expr(return_expr, ty_expect, loc),
            AExpr::If(if_expr) => self.convert_if_expr(if_expr, ty_expect, loc),
            AExpr::Match(match_expr) => self.convert_match_expr(match_expr, ty_expect, loc),
            AExpr::While(while_expr) => self.convert_while_expr(while_expr, ty_expect, loc),
            AExpr::Loop(loop_expr) => self.convert_loop_expr(loop_expr, ty_expect, loc),
        }
    }

    /// `&expr` を生成する。
    fn do_convert_lval(
        &mut self,
        expr_id: AExprId,
        expr: &AExpr,
        k_mut: KMut,
        ty_expect: TyExpect,
    ) -> AfterLval {
        let loc = Loc::new(self.doc, PLoc::Expr(expr_id));

        match expr {
            AExpr::Name(name) => self.convert_name_lval(*name, k_mut),
            AExpr::Record(expr) => self.convert_record_lval(expr, loc),
            AExpr::Field(field_expr) => self.convert_field_lval(field_expr, k_mut, loc),
            AExpr::Index(index_expr) => self.convert_index_lval(index_expr, ty_expect, loc),
            AExpr::UnaryOp(unary_op_expr) => {
                self.convert_unary_op_lval(unary_op_expr, ty_expect, loc)
            }
            _ => {
                // break や if など、左辺値と解釈可能な式は他にもある。いまのところ実装する必要はない

                let term = match self.convert_expr(expr_id, ty_expect) {
                    KTerm::Name(it) => it,
                    term => {
                        // FIXME: リテラルなら static を導入してそのアドレスを取る。

                        let temp = self.fresh_var("lval", loc);
                        self.nodes.push(new_let_node(term, temp, new_cont(), loc));
                        temp
                    }
                };
                let result = self.fresh_var("ref", loc);
                self.nodes
                    .push(new_ref_node(KTerm::Name(term), result, new_cont(), loc));
                KTerm::Name(result)
            }
        }
    }

    pub(crate) fn convert_expr(&mut self, expr_id: AExprId, ty_expect: TyExpect) -> AfterRval {
        let expr = expr_id.of(self.ast.exprs());
        self.do_convert_expr(expr_id, expr, ty_expect)
    }

    pub(crate) fn convert_lval(
        &mut self,
        expr_id: AExprId,
        k_mut: KMut,
        ty_expect: TyExpect,
    ) -> AfterLval {
        let expr = expr_id.of(self.ast.exprs());
        self.do_convert_lval(expr_id, expr, k_mut, ty_expect)
    }

    pub(crate) fn convert_expr_opt(
        &mut self,
        expr_id_opt: Option<AExprId>,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        match expr_id_opt {
            Some(expr_id) => self.convert_expr(expr_id, ty_expect),
            None => new_error_term(loc),
        }
    }

    pub(crate) fn convert_lval_opt(
        &mut self,
        expr_id_opt: Option<AExprId>,
        k_mut: KMut,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterLval {
        match expr_id_opt {
            Some(expr_id) => self.convert_lval(expr_id, k_mut, ty_expect),
            None => new_error_term(loc),
        }
    }
}

enum FieldExhaustivityError {
    NoSuchField {
        #[allow(unused)]
        item_index: usize,
    },
    Redundant {
        #[allow(unused)]
        item_index: usize,
    },
    Missed(KField),
}

fn calculate_field_ordering<'t, T>(
    items: &[T],
    fields: &[KField],
    field_arena: &'t KFieldArena,
    name_fn: impl Fn(&T) -> &'t str,
) -> Result<Vec<usize>, Vec<FieldExhaustivityError>> {
    // perm[item_index] = Some(field_index) or None
    let mut perm = vec![0; items.len()];
    // inverse[field_index] = Some(item_index) or None
    let mut inverse = vec![None; fields.len()];
    let mut errors = vec![];

    for (item_index, item) in items.iter().enumerate() {
        let name = name_fn(item);
        let i = match fields
            .iter()
            .position(|field| field.name(field_arena) == name)
        {
            Some(it) => it,
            None => {
                errors.push(FieldExhaustivityError::NoSuchField { item_index });
                continue;
            }
        };

        if let Some(first) = inverse[i] {
            errors.push(FieldExhaustivityError::Redundant { item_index: first });
            continue;
        }

        perm[item_index] = i;
        inverse[i] = Some(item_index);
    }

    for (item_index_opt, &field) in inverse.iter().zip(fields.iter()) {
        if item_index_opt.is_none() {
            errors.push(FieldExhaustivityError::Missed(field));
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    debug_assert_eq!(
        perm.iter()
            .filter(|&&i| i < fields.len())
            .collect::<HashSet<_>>()
            .len(),
        fields.len()
    );
    Ok(perm)
}

fn report_record_expr_errors(
    fields: &[KField],
    errors: &[FieldExhaustivityError],
    loc: Loc,
    mod_outline: &KModOutline,
    logger: &DocLogger,
) {
    // FIXME: フィールドの位置情報を取る。
    let mut missed = vec![];

    for error in errors {
        match error {
            FieldExhaustivityError::NoSuchField { .. } => {
                error_no_such_field(PLoc::from_loc(loc), logger);
            }
            FieldExhaustivityError::Redundant { .. } => {
                error_redundant_field(PLoc::from_loc(loc), logger);
            }
            FieldExhaustivityError::Missed(field) => missed.push(field),
        }
    }

    if !missed.is_empty() {
        error_missed_fields(
            fields.iter().map(|field| field.name(&mod_outline.fields)),
            PLoc::from_loc(loc),
            logger,
        );
    }
}

fn emit_unit_like_struct(
    k_struct: KStruct,
    result: KVarTerm,
    loc: Loc,
    nodes: &mut Vec<KNode>,
) -> AfterRval {
    let ty = KTy2::Struct(k_struct);

    nodes.push(new_record_node(ty, vec![], result, new_cont(), loc));
    KTerm::Name(result)
}
