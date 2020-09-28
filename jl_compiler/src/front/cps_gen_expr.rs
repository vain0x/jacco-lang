use crate::{
    cps::*,
    front::{cps_gen::*, cps_gen_lit::*, name_resolution::*},
    logs::*,
    parse::*,
    source::*,
};
use std::{collections::HashMap, collections::HashSet, iter::once, mem::take};

impl<'a> Xx<'a> {
    pub(crate) fn local_var_ty(&self, local_var: KLocalVar) -> KTy2 {
        local_var.ty(&self.local_vars)
    }

    pub(crate) fn const_ty(&self, k_const: KConst) -> KTy2 {
        k_const
            .of(&self.mod_outline.consts)
            .value_ty
            .to_ty2_poly(self.mod_outline)
    }

    pub(crate) fn static_var_ty(&self, static_var: KStaticVar) -> KTy2 {
        static_var
            .of(&self.mod_outline.static_vars)
            .ty
            .to_ty2_poly(self.mod_outline)
    }

    pub(crate) fn record_tag_ty(&self, k_struct: KStruct) -> KTy2 {
        k_struct
            .tag_ty(&self.mod_outline.structs, &self.mod_outline.struct_enums)
            .to_ty2_poly(self.mod_outline)
    }

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
        (
            KTerm::Name(break_label.result),
            self.local_var_ty(break_label.result.local_var),
        )
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

        match value {
            KLocalValue::LocalVar(local_var) => (
                KTerm::Name(KVarTerm { local_var, cause }),
                self.local_var_ty(local_var),
            ),
            KLocalValue::Const(k_const) => (KTerm::Const { k_const, loc }, self.const_ty(k_const)),
            KLocalValue::StaticVar(static_var) => (
                KTerm::StaticVar { static_var, loc },
                self.static_var_ty(static_var),
            ),
            KLocalValue::Fn(k_fn) => {
                // インスタンス化
                let ty = k_fn
                    .ty(&self.mod_outline.fns)
                    .to_ty2(self.mod_outline, &mut self.ty_env);
                (
                    KTerm::Fn {
                        k_fn,
                        ty: ty.clone(),
                        loc,
                    },
                    ty,
                )
            }
            KLocalValue::ExternFn(extern_fn) => {
                // インスタンス化
                let ty = extern_fn
                    .ty(&self.mod_outline.extern_fns)
                    .to_ty2(self.mod_outline, &mut self.ty_env);
                (KTerm::ExternFn { extern_fn, loc }, ty)
            }
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
            Some(it) => it,
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
                (KTerm::Name(result), KTy2::TODO)
            }
            KMut::Mut => {
                let result = self.fresh_var("refmut", cause);
                self.nodes
                    .push(new_ref_mut_node(term, result, new_cont(), loc));
                (KTerm::Name(result), KTy2::TODO)
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
            return (KTerm::TyProperty { kind, ty, loc }, KTy2::USIZE);
        }

        let value = match resolve_value_path(name, self.path_resolution_context()) {
            Some(it) => it,
            None => {
                error_unresolved_value(name.loc(), self.logger);
                return new_error_term(loc);
            }
        };

        match value {
            KLocalValue::Fn(k_fn) => {
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

                (
                    KTerm::Fn {
                        k_fn,
                        ty: ty.clone(),
                        loc,
                    },
                    ty,
                )
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
            let (term, _ty) = self.convert_expr_opt(field_expr.value_opt, TyExpect::Todo, loc);
            args[perm[i]] = term;
        }

        let result = self.fresh_var(&k_struct.of(&self.mod_outline.structs).name, loc);
        self.nodes
            .push(new_record_node(ty, args, result, new_cont(), loc));
        Some(result)
    }

    fn convert_record_expr(&mut self, expr: &ARecordExpr, loc: Loc) -> AfterRval {
        match self.do_convert_record_expr(expr, loc) {
            Some(result) => (KTerm::Name(result), KTy2::TODO),
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
        (KTerm::Name(result), KTy2::TODO)
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

        let (field_ptr, _ty) = self.convert_field_lval(expr, KMut::Const, loc);
        self.nodes
            .push(new_deref_node(field_ptr, result, new_cont(), loc));
        (KTerm::Name(result), KTy2::TODO)
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

        let (left, _ty) = self.convert_lval(expr.left, k_mut, TyExpect::Todo);
        self.nodes.push(new_field_node(
            left,
            name,
            field_loc,
            k_mut,
            result,
            new_cont(),
            loc,
        ));
        (KTerm::Name(result), KTy2::TODO)
    }

    fn convert_call_expr(
        &mut self,
        call_expr: &ACallLikeExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let mut ty_rule = CallExprRule::new(self.logger);

        let result = self.fresh_var("call_result", loc);
        let (left, fn_ty) = self.convert_expr(call_expr.left, TyExpect::Unknown);
        ty_rule.verify_fn_ty(&fn_ty, &self.ty_env);

        let mut args = Vec::with_capacity(call_expr.args.len() + 1);
        args.push(left);
        args.extend(call_expr.args.iter().enumerate().map(|(i, arg)| {
            let (term, arg_ty) = self.convert_expr(arg, ty_rule.arg_ty_expect(i));
            ty_rule.verify_arg_ty(i, &arg_ty, &self.ty_env);
            term
        }));
        let result_ty = ty_rule.to_result_ty();

        self.nodes
            .push(new_call_node(args, result, new_cont(), loc));
        *result.ty_mut(&mut self.local_vars) = result_ty.clone();
        (KTerm::Name(result), result_ty)
    }

    // `a[i]` ==> `*(a + i)`
    fn convert_index_expr(
        &mut self,
        expr: &ACallLikeExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let (ptr, _ty) = self.convert_index_lval(expr, TyExpect::Todo, loc);

        let result = self.fresh_var("index_result", loc);
        self.nodes
            .push(new_deref_node(ptr, result, new_cont(), loc));
        (KTerm::Name(result), KTy2::TODO)
    }

    // `&a[i]` ==> `a + i`
    fn convert_index_lval(
        &mut self,
        expr: &ACallLikeExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterLval {
        let (left, _ty) = self.convert_expr(expr.left, TyExpect::Todo);
        let (right, _ty) = match expr.args.iter().next() {
            Some(right) if expr.args.len() == 1 => self.convert_expr(right, TyExpect::Todo),
            _ => new_error_term(loc),
        };

        let result = self.fresh_var("indexed_ptr", loc);
        self.nodes
            .push(new_add_node(left, right, result, new_cont(), loc));
        (KTerm::Name(result), KTy2::TODO)
    }

    fn convert_cast_expr(&mut self, expr: &ACastExpr, _ty_expect: TyExpect, loc: Loc) -> AfterRval {
        let ty = self
            .ty_resolver()
            .convert_ty_opt(expr.ty_opt)
            .to_ty2_poly(self.mod_outline);
        let (arg, _ty) = self.convert_expr(expr.left, TyExpect::from(&ty));

        let result = self.fresh_var("cast", loc);
        self.nodes
            .push(new_cast_node(ty, arg, result, new_cont(), loc));
        (KTerm::Name(result), KTy2::TODO)
    }

    fn convert_unary_op_expr(
        &mut self,
        expr: &AUnaryOpExpr,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        match expr.op {
            PUnaryOp::Deref => {
                // FIXME: ty_expect に和型 (*T | *mut T) を指定する?
                let (arg, arg_ty) = self.convert_expr_opt(expr.arg_opt, ty_expect, loc);
                let (_mut, value_ty) = match arg_ty.as_ptr(&self.ty_env) {
                    Some(it) => it,
                    None => {
                        // FIXME: error
                        (KMut::Const, KTy2::Never)
                    }
                };

                let result = self.fresh_var("deref", loc);
                self.nodes
                    .push(new_deref_node(arg, result, new_cont(), loc));
                (KTerm::Name(result), value_ty)
            }
            PUnaryOp::Ref => {
                let k_mut = expr.mut_opt.unwrap_or(KMut::Const);

                let arg_ty_expect = ty_expect.try_unwrap_ptr(&self.ty_env);
                self.convert_lval_opt(expr.arg_opt, k_mut, arg_ty_expect, loc)
            }
            PUnaryOp::Minus => {
                let arg_ty_expect = ty_expect.meet(TyExpect::Number);
                let (arg, arg_ty) = self.convert_expr_opt(expr.arg_opt, arg_ty_expect, loc);
                let result_ty = arg_ty;

                let result = self.fresh_var("minus", loc);
                self.nodes
                    .push(new_minus_node(arg, result, new_cont(), loc));
                (KTerm::Name(result), result_ty)
            }
            PUnaryOp::Not => {
                let arg_ty_expect = ty_expect.meet(TyExpect::BoolOrInt);
                let (arg, arg_ty) = self.convert_expr_opt(expr.arg_opt, arg_ty_expect, loc);
                let result_ty = arg_ty;

                let result = self.fresh_var("not", loc);
                self.nodes.push(new_not_node(arg, result, new_cont(), loc));
                (KTerm::Name(result), result_ty)
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
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let left_ty_expect = ty_expect;
        let (left, left_ty) = self.convert_lval(expr.left, KMut::Mut, left_ty_expect);

        let right_ty_expect = TyExpect::Exact(left_ty);
        let (right, _ty) = self.convert_expr_opt(expr.right_opt, right_ty_expect, loc);

        self.nodes
            .push(new_assignment_node(prim, left, right, new_cont(), loc));
        new_unit_term(loc)
    }

    fn do_convert_add_expr(
        &mut self,
        expr: &ABinaryOpExpr,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let mut ty_rule = AddExprRule::new(ty_expect, self.logger);

        let (left, left_ty) = self.convert_expr(expr.left, ty_rule.left_ty_expect());
        ty_rule.verify_left_ty(&left_ty, &self.ty_env);

        let right_ty_expect = ty_rule.right_ty_expect();
        let (right, right_ty) = self.convert_expr_opt(expr.right_opt, right_ty_expect, loc);
        ty_rule.verify_right_ty(&right_ty, &self.ty_env);

        let result = self.fresh_var("add", loc);
        let result_ty = ty_rule.to_result_ty();
        *result.ty_mut(&mut self.local_vars) = result_ty.clone();

        self.nodes.push(new_basic_binary_op_node(
            KPrim::Add,
            left,
            right,
            result,
            new_cont(),
            loc,
        ));
        (KTerm::Name(result), result_ty)
    }

    fn do_convert_basic_binary_op_expr(
        &mut self,
        prim: KPrim,
        expr: &ABinaryOpExpr,
        loc: Loc,
    ) -> AfterRval {
        let (left, _ty) = self.convert_expr(expr.left, TyExpect::Todo);
        let (right, _ty) = self.convert_expr_opt(expr.right_opt, TyExpect::Todo, loc);

        let result = self.fresh_var(&prim.hint_str(), loc);
        self.nodes.push(new_basic_binary_op_node(
            prim,
            left,
            right,
            result,
            new_cont(),
            loc,
        ));
        (KTerm::Name(result), KTy2::TODO)
    }

    // `p && q` ==> `if p { q } else { false }`
    fn do_convert_log_and_expr(
        &mut self,
        expr: &ABinaryOpExpr,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        self.do_convert_if_expr(
            |xx| xx.convert_expr(expr.left, TyExpect::bool()),
            |xx, _| xx.convert_expr_opt(expr.right_opt, TyExpect::bool(), loc),
            |_, _| new_false_term(loc),
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
            |xx| xx.convert_expr(expr.left, TyExpect::bool()),
            |_, _| new_true_term(loc),
            |xx, ty_expect| xx.convert_expr_opt(expr.right_opt, ty_expect, loc),
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
        let on_assign = |prim: KPrim, ty_expect: TyExpect, xx| {
            Xx::do_convert_assignment_expr(xx, prim, expr, ty_expect, loc)
        };
        let on_basic = |prim: KPrim, xx| Xx::do_convert_basic_binary_op_expr(xx, prim, expr, loc);
        let on_bit = on_basic;
        let on_comparison = on_basic;

        match expr.op {
            PBinaryOp::Assign => on_assign(KPrim::Assign, ty_expect, xx),
            PBinaryOp::AddAssign => on_assign(KPrim::AddAssign, ty_expect, xx),
            PBinaryOp::SubAssign => on_assign(KPrim::SubAssign, ty_expect, xx),
            PBinaryOp::MulAssign => on_assign(KPrim::MulAssign, ty_expect, xx),
            PBinaryOp::DivAssign => on_assign(KPrim::DivAssign, ty_expect, xx),
            PBinaryOp::ModuloAssign => on_assign(KPrim::ModuloAssign, ty_expect, xx),
            PBinaryOp::BitAndAssign => on_assign(KPrim::BitAndAssign, ty_expect, xx),
            PBinaryOp::BitOrAssign => on_assign(KPrim::BitOrAssign, ty_expect, xx),
            PBinaryOp::BitXorAssign => on_assign(KPrim::BitXorAssign, ty_expect, xx),
            PBinaryOp::LeftShiftAssign => on_assign(KPrim::LeftShiftAssign, ty_expect, xx),
            PBinaryOp::RightShiftAssign => on_assign(KPrim::RightShiftAssign, ty_expect, xx),
            PBinaryOp::Add => xx.do_convert_add_expr(expr, ty_expect, loc),
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

    fn convert_block_expr(&mut self, decls: ADeclIds, ty_expect: TyExpect, loc: Loc) -> AfterRval {
        self.convert_decls(decls, ty_expect)
            .unwrap_or_else(|| new_unit_term(loc))
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
                return new_error_term(loc).0;
            }
        };

        let (arg, _ty) = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);
        self.nodes
            .push(new_jump_node(label, vec![arg], new_cont(), loc));
        new_never_term(loc).0
    }

    fn convert_continue_expr(&mut self, _ty_expect: TyExpect, loc: Loc) -> AfterJump {
        let label_opt = self.loop_opt.as_ref().map(|data| data.continue_label);
        let label = match label_opt {
            Some(it) => it,
            None => {
                error_continue_out_of_loop(PLoc::from_loc(loc), self.logger);
                return new_error_term(loc).0;
            }
        };

        self.nodes.push(new_jump_node(
            label,
            vec![KTerm::Unit { loc }],
            new_cont(),
            loc,
        ));
        new_never_term(loc).0
    }

    fn convert_return_expr(
        &mut self,
        expr: &AJumpExpr,
        _ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterJump {
        let (arg, _ty) = self.convert_expr_opt(expr.arg_opt, TyExpect::Todo, loc);

        let node = match self.fn_opt {
            Some(k_fn) => new_return_node(k_fn, arg, loc),
            None => {
                error_return_out_of_fn(PLoc::from_loc(loc), self.logger);
                new_error_node(loc)
            }
        };

        self.nodes.push(node);
        new_never_term(loc).0
    }

    fn do_convert_if_expr(
        &mut self,
        cond_fn: impl FnOnce(&mut Xx) -> AfterRval,
        body_fn: impl FnOnce(&mut Xx, TyExpect) -> AfterRval,
        alt_fn: impl FnOnce(&mut Xx, TyExpect) -> AfterRval,
        ty_expect: TyExpect,
        loc: Loc,
    ) -> AfterRval {
        let result = self.fresh_var("if_result", loc);
        // FIXME: next → if_next
        let next = self.new_break_label("next", result);

        let mut ty_rule = IfRule::new(ty_expect, self.logger);
        let mut body_ty = KTy2::DEFAULT;
        let mut alt_ty = KTy2::Unit;

        self.do_with_break(next, |xx, break_label| {
            let (cond, cond_ty) = cond_fn(xx);
            ty_rule.verify_cond_ty(&cond_ty, &xx.ty_env);
            xx.nodes
                .push(new_if_node(cond, new_cont(), new_cont(), loc));

            xx.do_in_branch(|xx| {
                let (body, body_ty1) = body_fn(xx, ty_rule.body_ty_expect());
                body_ty = body_ty1;
                ty_rule.verify_body_ty(&body_ty);
                xx.nodes.push(new_jump_tail(break_label, once(body), loc));
            });

            xx.do_in_branch(|xx| {
                let (alt, alt_ty1) = alt_fn(xx, ty_rule.alt_ty_expect());
                alt_ty = alt_ty1;
                ty_rule.verify_alt_ty(&alt_ty);
                xx.nodes.push(new_jump_tail(break_label, once(alt), loc));
                ty_rule.verify_cond_ty(&cond_ty, &xx.ty_env);
            });

            let _result_ty = ty_rule.to_result_ty(&xx.ty_env);
            // *result.ty_mut(&mut xx.local_vars) = result_ty;
        })
    }

    fn convert_if_expr(&mut self, expr: &AIfExpr, ty_expect: TyExpect, loc: Loc) -> AfterRval {
        self.do_convert_if_expr(
            |xx| xx.convert_expr_opt(expr.cond_opt, TyExpect::bool(), loc),
            |xx, ty_expect| xx.convert_expr_opt(expr.body_opt, ty_expect, loc),
            |xx, ty_expect| xx.convert_expr_opt(expr.alt_opt, ty_expect, loc),
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
        let (cond, _ty) = self.convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc);
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
                        Branch::Case(term, _ty) => term,
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
                    let (body, _ty) = xx.convert_expr_opt(arm.body_opt, TyExpect::Todo, loc);
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
        self.do_in_loop("while_result", loc, |xx, _, break_label, continue_label| {
            let (cond, _ty) = xx.convert_expr_opt(expr.cond_opt, TyExpect::Todo, loc);
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
                    .push(new_jump_tail(break_label, vec![KTerm::Unit { loc }], loc));
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
        log::trace!(
            "expr=#{} ty_expect={}",
            expr_id.to_index(),
            ty_expect.display(&self.ty_env, self.mod_outline)
        );

        match expr {
            AExpr::Unit => new_unit_term(loc),
            AExpr::True => new_true_term(loc),
            AExpr::False => new_false_term(loc),
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
            AExpr::Break(break_expr) => {
                self.convert_break_expr(break_expr, ty_expect, loc);
                new_never_term(loc)
            }
            AExpr::Continue => {
                self.convert_continue_expr(ty_expect, loc);
                new_never_term(loc)
            }
            AExpr::Return(return_expr) => {
                self.convert_return_expr(return_expr, ty_expect, loc);
                new_never_term(loc)
            }
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

                let term = match self.convert_expr(expr_id, ty_expect).0 {
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
                (KTerm::Name(result), KTy2::TODO)
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

    nodes.push(new_record_node(ty.clone(), vec![], result, new_cont(), loc));
    (KTerm::Name(result), ty)
}

struct CallExprRule<'a> {
    #[allow(unused)]
    logger: &'a DocLogger,
    arg_tys: Vec<KTy2>,
    result_ty_opt: Option<KTy2>,
}

impl<'a> CallExprRule<'a> {
    fn new(logger: &'a DocLogger) -> Self {
        Self {
            logger,
            arg_tys: vec![],
            result_ty_opt: None,
        }
    }

    fn verify_fn_ty(&mut self, fn_ty: &'a KTy2, ty_env: &KTyEnv) {
        KModOutline::using_for_debug(|mod_outline_opt| {
            log::trace!(
                "call fn: {}",
                fn_ty.display(&ty_env, mod_outline_opt.unwrap()),
            )
        });

        match fn_ty.as_fn(ty_env) {
            Some((arg_tys, result_ty)) => {
                self.arg_tys = arg_tys;
                self.result_ty_opt = Some(result_ty);
            }
            None => {
                // FIXME: error
            }
        }
    }

    fn arg_ty_expect(&self, i: usize) -> TyExpect {
        match self.arg_tys.get(i) {
            Some(ty) => TyExpect::Exact(ty.clone()),
            None => TyExpect::Todo,
        }
    }

    fn verify_arg_ty(&mut self, i: usize, arg_ty: &KTy2, ty_env: &KTyEnv) {
        KModOutline::using_for_debug(|mod_outline_opt| {
            log::trace!(
                "call arg#{}: {}",
                i,
                arg_ty.display(&ty_env, mod_outline_opt.unwrap()),
            )
        });
    }

    fn to_result_ty(mut self) -> KTy2 {
        // FIXME: 後続の型検査に任せるため unresolved にする
        self.result_ty_opt.take().unwrap_or(KTy2::Unresolved {
            cause: KTyCause::Default,
        })
    }
}

#[derive(Debug)]
enum AddExprRuleKind {
    Number,
    PtrAdd,
}

struct AddExprRule<'a> {
    ty_expect: TyExpect,
    #[allow(unused)]
    logger: &'a DocLogger,
    kind_opt: Option<AddExprRuleKind>,
    left_ty_opt: Option<KTy2>,
}

impl<'a> AddExprRule<'a> {
    fn new(ty_expect: TyExpect, logger: &'a DocLogger) -> Self {
        AddExprRule {
            ty_expect,
            logger,
            kind_opt: None,
            left_ty_opt: None,
        }
    }

    fn left_ty_expect(&self) -> TyExpect {
        self.ty_expect.clone().meet(TyExpect::NumberOrPtr)
    }

    fn verify_left_ty(&mut self, left_ty: &KTy2, ty_env: &KTyEnv) {
        let kind = if left_ty.is_ptr(&ty_env) {
            AddExprRuleKind::PtrAdd
        } else {
            AddExprRuleKind::Number
        };
        self.kind_opt = Some(kind);
        self.left_ty_opt = Some(left_ty.clone());

        // FIXME: left_ty: number or ptr でなければエラー
        KModOutline::using_for_debug(|mod_outline_opt| {
            log::trace!(
                "add({:?}) left_ty={} (expected {})",
                self.kind_opt,
                left_ty.display(ty_env, mod_outline_opt.unwrap()),
                self.ty_expect.display(ty_env, mod_outline_opt.unwrap()),
            );
        });
    }

    fn right_ty_expect(&self) -> TyExpect {
        let other = match self.kind_opt.as_ref().unwrap() {
            AddExprRuleKind::Number => TyExpect::Exact(self.left_ty_opt.clone().unwrap()),
            AddExprRuleKind::PtrAdd => TyExpect::IsizeOrUsize,
        };
        self.ty_expect.clone().meet(other)
    }

    fn verify_right_ty(&self, right_ty: &KTy2, ty_env: &KTyEnv) {
        // FIXME: unify with left_ty

        KModOutline::using_for_debug(|mod_outline_opt| {
            log::trace!(
                "add right_ty={}",
                right_ty.display(ty_env, mod_outline_opt.unwrap())
            );
        });
    }

    fn to_result_ty(&mut self) -> KTy2 {
        self.left_ty_opt.take().unwrap()
    }
}

struct IfRule<'a> {
    ty_expect: TyExpect,
    #[allow(unused)]
    logger: &'a DocLogger,
    body_ty_opt: Option<KTy2>,
    alt_ty: KTy2,
}

impl<'a> IfRule<'a> {
    fn new(ty_expect: TyExpect, logger: &'a DocLogger) -> Self {
        Self {
            ty_expect,
            logger,
            body_ty_opt: None,
            alt_ty: KTy2::Unit,
        }
    }

    fn verify_cond_ty(&self, cond_ty: &KTy2, ty_env: &KTyEnv) {
        if !cond_ty.is_bool(ty_env) {
            // FIXME: error
        }
    }

    fn body_ty_expect(&self) -> TyExpect {
        self.ty_expect.clone()
    }

    fn verify_body_ty(&mut self, body_ty: &KTy2) {
        self.body_ty_opt = Some(body_ty.clone());
    }

    fn alt_ty_expect(&self) -> TyExpect {
        self.ty_expect
            .clone()
            .meet(TyExpect::Exact(self.body_ty_opt.clone().unwrap()))
    }

    fn verify_alt_ty(&mut self, alt_ty: &KTy2) {
        // FIXME: body_ty と互換性がなければエラー？
        self.alt_ty = alt_ty.clone();
    }

    fn to_result_ty(self, ty_env: &KTyEnv) -> KTy2 {
        let body_ty = self.body_ty_opt.unwrap();
        let alt_ty = &self.alt_ty;
        let ty = body_ty.join(alt_ty, ty_env);

        KModOutline::using_for_debug(|mod_outline_opt| {
            let mod_outline = mod_outline_opt.unwrap();
            log::trace!(
                "if(body: {}, alt: {}): {}",
                body_ty.display(ty_env, mod_outline),
                alt_ty.display(ty_env, mod_outline),
                ty.display(ty_env, mod_outline),
            );
        });

        ty
    }
}
