//! 構文木から CPS ノードのもとになる命令列を生成する処理

#![allow(unused)]

use super::{env::Env, name_resolution::*, NName};
use crate::cps::*;
use crate::parse::*;
use crate::{
    front::NameResolution,
    logs::DocLogger,
    source::{Doc, HaveLoc, Loc},
    token::{eval_number, LitErr},
    utils::VecArena,
};
use log::{error, trace};
use std::{
    borrow::Cow,
    collections::HashMap,
    iter::once,
    mem::{replace, take},
};

#[derive(Clone)]
struct KLoopData {
    break_label: KLabel,
    continue_label: KLabel,
}

/// Code generation context.
struct Gx<'a> {
    mod_outline: KModOutline,
    current_commands: Vec<KCommand>,
    current_locals: KLocalArena,
    current_loops: VecArena<NLoopTag, KLoopData>,
    current_labels: KLabelArena,
    fns: KFnArena,
    fn_loops: VecArena<KFnTag, NLoopArena>,
    extern_fns: KExternFnArena,
    tokens: &'a PTokens,
    names: &'a PNameArena,
    name_res: &'a VecArena<PNameTag, NName>,
    logger: DocLogger,
}

impl<'a> Gx<'a> {
    fn new(
        tokens: &'a PTokens,
        names: &'a PNameArena,
        name_res: &'a VecArena<PNameTag, NName>,
        logger: DocLogger,
    ) -> Self {
        Self {
            mod_outline: Default::default(),
            current_commands: Default::default(),
            current_locals: Default::default(),
            current_loops: Default::default(),
            current_labels: Default::default(),
            fns: Default::default(),
            fn_loops: Default::default(),
            extern_fns: Default::default(),
            tokens,
            names,
            name_res,
            logger,
        }
    }

    fn fresh_symbol(&mut self, hint: &str, loc: Loc) -> KSymbol {
        let local = self
            .current_locals
            .alloc(KLocalData::new(hint.to_string(), loc));

        KSymbol { local, loc }
    }

    fn fresh_label(&mut self, hint: &str, _loc: Loc) -> KLabel {
        let name = hint.to_string();

        self.current_labels.alloc(KLabelData {
            name,
            params: vec![],
            body: KNode::default(),
        })
    }

    fn push(&mut self, command: KCommand) {
        self.current_commands.push(command);
    }

    fn push_label(&mut self, label: KLabel, params: Vec<KSymbol>) {
        self.push(KCommand::Label { label, params })
    }

    // ちょうど1つの継続を持つプリミティブノードを生成する。
    fn push_prim_1(&mut self, prim: KPrim, args: Vec<KTerm>, result: KSymbol) {
        let loc = result.loc;
        self.push(KCommand::Node {
            prim,
            tys: vec![],
            args,
            result_opt: Some(result),
            cont_count: 1,
            loc,
        });
    }

    fn do_push_jump(
        &mut self,
        label: KLabel,
        args: impl IntoIterator<Item = KTerm>,
        cont_count: usize,
        loc: Loc,
    ) {
        self.push(KCommand::Node {
            prim: KPrim::Jump,
            tys: vec![],
            args: once(KTerm::Label { label, loc }).chain(args).collect(),
            result_opt: None,
            cont_count,
            loc,
        });
    }

    fn push_jump(&mut self, label: KLabel, args: impl IntoIterator<Item = KTerm>, loc: Loc) {
        self.do_push_jump(label, args, 0, loc);
    }

    fn push_jump_with_cont(
        &mut self,
        label: KLabel,
        args: impl IntoIterator<Item = KTerm>,
        loc: Loc,
    ) {
        self.do_push_jump(label, args, 1, loc);
    }
}

fn new_false_term(loc: Loc) -> KTerm {
    KTerm::False { loc }
}

fn new_true_term(loc: Loc) -> KTerm {
    KTerm::True { loc }
}

fn new_unit_term(loc: Loc) -> KTerm {
    KTerm::Unit { loc }
}

fn new_never_term(loc: Loc) -> KTerm {
    // FIXME: the type is ! (never)
    KTerm::Unit { loc }
}

fn error_token(token: PToken, message: impl Into<String>, gx: &Gx) {
    let loc = PLoc::new(token);
    gx.logger.error(loc, message);
}

fn error_node(node: Loc, message: impl Into<String>, gx: &Gx) {
    log::error!("{} {:?}", message.into(), node);
    let loc = PLoc::new(PToken::from_index(0));
    gx.logger.error(loc, "<cps_conversion::error_node>");
}

fn fresh_loop_labels(loc: Loc, gx: &mut Gx) -> KLoopData {
    let continue_label = gx.fresh_label("continue_", loc);
    let break_label = gx.fresh_label("next", loc);
    KLoopData {
        break_label,
        continue_label,
    }
}

fn emit_unary_op(prim: KPrim, arg_opt: Option<&PExpr>, loc: Loc, gx: &mut Gx) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), loc);

    let arg = gen_expr(arg_opt.unwrap(), gx);

    gx.push_prim_1(prim, vec![arg], result.clone());

    KTerm::Name(result)
}

fn emit_compound_assign(
    prim: KPrim,
    left: &PExpr,
    right_opt: Option<&PExpr>,
    loc: Loc,
    gx: &mut Gx,
) -> KTerm {
    let left = gen_expr_lval(left, KMut::Mut, loc, gx);
    let right = gen_expr(right_opt.unwrap(), gx);

    gx.push(KCommand::Node {
        prim,
        tys: vec![],
        args: vec![left, right],
        result_opt: None,
        cont_count: 1,
        loc,
    });

    new_unit_term(loc)
}

fn emit_binary_op(
    prim: KPrim,
    left: &PExpr,
    right_opt: Option<&PExpr>,
    loc: Loc,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), loc);

    let left = gen_expr(left, gx);
    let right = gen_expr(right_opt.unwrap(), gx);

    gx.push_prim_1(prim, vec![left, right], result.clone());

    KTerm::Name(result)
}

fn emit_if(
    cond: &PExpr,
    gen_body: impl FnOnce(&mut Gx) -> KTerm,
    gen_alt: impl FnOnce(&mut Gx) -> KTerm,
    loc: Loc,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol("if_result", loc);
    let next_label = gx.fresh_label("next", loc);

    let k_cond = gen_expr(cond, gx);

    gx.push(KCommand::Node {
        prim: KPrim::If,
        tys: vec![],
        args: vec![k_cond],
        result_opt: None,
        cont_count: 2,
        loc,
    });

    // body
    {
        let body = gen_body(gx);
        gx.push_jump(next_label, vec![body], loc);
    }

    // alt
    {
        let alt = gen_alt(gx);
        gx.push_jump(next_label, vec![alt], loc);
    }

    gx.push_label(next_label, vec![result.clone()]);

    KTerm::Name(result)
}

fn gen_mut(p_mut: Option<&PMut>) -> KMut {
    match p_mut {
        Some(&(k_mut, _)) => k_mut,
        None => KMut::Const,
    }
}

// ローカル型環境を引数に取るかもしれない
pub(crate) fn gen_ty_name(p_name: PName, name_res: &VecArena<PNameTag, NName>) -> KTy {
    match *p_name.of(name_res) {
        NName::I8 => KTy::I8,
        NName::I16 => KTy::I16,
        NName::I32 => KTy::I32,
        NName::I64 => KTy::I64,
        NName::Isize => KTy::ISIZE,
        NName::U8 => KTy::U8,
        NName::U16 => KTy::U16,
        NName::U32 => KTy::U32,
        NName::U64 => KTy::U64,
        NName::Usize => KTy::USIZE,
        NName::F32 => KTy::F32,
        NName::F64 => KTy::F64,
        NName::C8 => KTy::C8,
        NName::C16 => KTy::C16,
        NName::C32 => KTy::C32,
        NName::Bool => KTy::BOOL,
        NName::Alias(alias) => KTy::Alias(alias),
        NName::Enum(n_enum) => KTy::Enum(n_enum),
        NName::Struct(n_struct) => KTy::Struct(n_struct),
        _ => KTy::Unresolved,
    }
}

pub(crate) fn gen_ty(ty: &PTy, name_res: &VecArena<PNameTag, NName>) -> KTy {
    match ty {
        PTy::Name(name) => gen_ty_name(*name, name_res),
        PTy::Never(_) => KTy::Never,
        PTy::Unit(_) => KTy::Unit,
        PTy::Ptr(PPtrTy {
            ty_opt, mut_opt, ..
        }) => {
            let k_mut = gen_mut(mut_opt.as_ref());
            let base_ty = match ty_opt.as_deref() {
                Some(ty) => gen_ty(ty, name_res),
                None => return KTy::Unresolved,
            };
            base_ty.into_ptr(k_mut)
        }
    }
}

fn convert_number_lit(token: PToken, tokens: &PTokens, logger: &DocLogger) -> KTerm {
    let result = eval_number(token.text(tokens));

    match result {
        Ok((_, number_ty)) => {
            let (text, loc) = token.decompose(&tokens);
            let ty = KTy2::Number(number_ty);
            match number_ty {
                KNumberTy::F32 | KNumberTy::F64 | KNumberTy::FNN => KTerm::Float { text, ty, loc },
                KNumberTy::C8 | KNumberTy::C16 | KNumberTy::C32 | KNumberTy::CNN => {
                    KTerm::Char { text, ty, loc }
                }
                KNumberTy::UNN => {
                    // FIXME: 後続のパスが UNN をうまく処理できなくて、unsigned long long になってしまう。いまのところ、ここで i32 にしておく
                    KTerm::Int {
                        text,
                        ty: KTy2::I32,
                        loc,
                    }
                }
                _ => KTerm::Int { text, ty, loc },
            }
        }
        Err(err) => {
            let message = match err {
                LitErr::Flow => "不正な値です",
                LitErr::UnknownSuffix => "不正なサフィックスです",
            };
            logger.error(PLoc::new(token), message);
            new_never_term(token.loc(tokens))
        }
    }
}

fn gen_number_lit(token: PToken, gx: &Gx) -> KTerm {
    convert_number_lit(token, &gx.tokens, &gx.logger)
}

fn gen_name(name: PName, gx: &mut Gx) -> KSymbolExt {
    let token = name.of(&gx.names).token;
    let n_name = name.of(&gx.name_res);
    let (name, loc) = (name.text(&gx.names).to_string(), name.loc());

    match *n_name {
        NName::Alias(alias) => KSymbolExt::Alias { alias, loc },
        NName::LocalVar(local) => {
            let local_data = &mut gx.current_locals[local];
            local_data.name = name.to_string();
            local_data.is_alive = true;

            KSymbolExt::Symbol(KSymbol { local, loc })
        }
        NName::Fn(k_fn) => KSymbolExt::Fn(k_fn),
        NName::ExternFn(extern_fn) => KSymbolExt::ExternFn(extern_fn),
        NName::Const(n_const) => KSymbolExt::Const(n_const),
        NName::StaticVar(n_static_var) => KSymbolExt::StaticVar(n_static_var),
        NName::Struct(n_struct) if n_struct.fields(&gx.mod_outline.structs).is_empty() => {
            KSymbolExt::UnitLikeStruct {
                k_struct: n_struct,
                loc,
            }
        }
        NName::I8
        | NName::I16
        | NName::I32
        | NName::I64
        | NName::Isize
        | NName::U8
        | NName::U16
        | NName::U32
        | NName::U64
        | NName::Usize
        | NName::F32
        | NName::F64
        | NName::C8
        | NName::C16
        | NName::C32
        | NName::Bool
        | NName::Enum(_)
        | NName::Struct(_) => {
            error_token(token, "型の名前です。", gx);
            // FIXME: 適切にハンドル？
            KSymbolExt::Symbol(gx.fresh_symbol(&name, loc))
        }
        NName::Unresolved => {
            // Unresolved ならエラーなのでコード生成には来ないはず。
            error!("cannot gen_name {:?}", (&name, loc, n_name));
            KSymbolExt::Unresolved
        }
    }
}

fn gen_constant(expr: &PExpr, gx: &mut Gx) -> Option<KConstValue> {
    match expr {
        PExpr::Number(PNumberExpr { token }) => match eval_number(token.text(&gx.tokens)) {
            Ok((value, _)) => {
                // FIXME: 型を見る？
                let value = match value {
                    KNumber::INN(value) => KConstValue::I64(value),
                    KNumber::UNN(value) => KConstValue::Usize(value as usize),
                    KNumber::FNN(value) => KConstValue::F64(value),
                    KNumber::CNN(value) => KConstValue::I32(value as i32),
                };
                Some(value)
            }
            Err(err) => {
                // FIXME: エラーハンドリング
                log::error!("{:?}", err);
                None
            }
        },
        PExpr::True(_) => Some(KConstValue::Bool(true)),
        PExpr::False(_) => Some(KConstValue::Bool(false)),
        PExpr::Name(name) => match gen_name(*name, gx) {
            KSymbolExt::Const(k_const) => k_const.of(&gx.mod_outline.consts).value_opt.clone(),
            _ => None,
        },
        _ => {
            // FIXME: 実装
            log::error!("未実装の定数式です {:?}", expr);
            None
        }
    }
}

fn gen_const_variant(decl: &PConstVariantDecl, value_slot: &mut usize, gx: &mut Gx) -> KConst {
    let PConstVariantDecl {
        name, value_opt, ..
    } = decl;

    let k_const = match *name.of(&gx.name_res) {
        NName::Const(n_const) => n_const,
        _ => unreachable!(),
    };
    assert_eq!(
        name.text(&gx.names),
        k_const.of(&gx.mod_outline.consts).name
    );

    if let Some(value) = value_opt.as_ref() {
        let loc = value.loc();
        match gen_constant(value, gx) {
            Some(value) => {
                // FIXME: 値を設定できるのはすべてのバリアントが const なときだけ
                *value_slot = value.cast_as_usize()
            }
            None => error_node(loc, "定数式として不正です", gx),
        }
    }

    k_const.of_mut(&mut gx.mod_outline.consts).value_opt = Some(KConstValue::Usize(*value_slot));

    k_const
}

fn gen_record_variant(decl: &PRecordVariantDecl, gx: &mut Gx) -> KStruct {
    let PRecordVariantDecl { name, .. } = decl;

    let k_struct = match *name.of(&gx.name_res) {
        NName::Struct(n_struct) => n_struct,
        n_name_opt => unreachable!("{:?}", n_name_opt),
    };
    assert_eq!(name.text(&gx.names), k_struct.name(&gx.mod_outline.structs));

    k_struct
}

fn gen_variant(decl: &PVariantDecl, value_slot: &mut usize, gx: &mut Gx) -> KVariant {
    match decl {
        PVariantDecl::Const(decl) => KVariant::Const(gen_const_variant(decl, value_slot, gx)),
        PVariantDecl::Record(decl) => KVariant::Record(gen_record_variant(decl, gx)),
    }
}

/// 式を左辺値とみなして変換する。(結果として、ポインタ型の項を期待している。)
fn gen_expr_lval(expr: &PExpr, k_mut: KMut, loc: Loc, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Tuple(PTupleExpr { arg_list }) if !arg_list.is_tuple() => {
            let arg = &arg_list.args[0];
            gen_expr_lval(&arg.expr, k_mut, loc, gx)
        }
        PExpr::Index(PIndexExpr { left, arg_list }) => {
            // a[i] ==> *(a + i)

            let loc = arg_list.left_paren.loc(&gx.tokens);
            let indexed_ptr = gx.fresh_symbol("indexed_ptr", loc);

            if arg_list.args.len() != 1 {
                error_node(
                    loc,
                    "0個や2個以上の引数を持つインデックス式は未実装です",
                    gx,
                );
                return new_never_term(loc);
            }

            let k_left = gen_expr(&left, gx);
            let k_arg = gen_expr(&arg_list.args[0].expr, gx);
            gx.push_prim_1(KPrim::Add, vec![k_left, k_arg], indexed_ptr.clone());

            KTerm::Name(indexed_ptr)
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            op: PUnaryOp::Deref,
            arg_opt: Some(arg),
            ..
        }) => {
            // &*x ==> x
            gen_expr(arg.as_ref(), gx)
        }
        PExpr::DotField(PDotFieldExpr { left, name_opt, .. }) => {
            // x.foo は `&(&x)->foo` のような形にコンパイルする。
            let (name, loc) = name_opt
                .as_ref()
                .map(|name| (name.text(&gx.tokens).to_string(), name.loc(&gx.tokens)))
                .unwrap();
            let result = gx.fresh_symbol(&format!("{}_ptr", name), loc);

            let left = gen_expr_lval(left.as_ref(), k_mut, loc, gx);
            let field = KTerm::FieldTag(KFieldTag { name, loc });

            let get_field_prim = match k_mut {
                KMut::Const => KPrim::GetField,
                KMut::Mut => KPrim::GetFieldMut,
            };
            gx.push_prim_1(get_field_prim, vec![left, field], result.clone());

            KTerm::Name(result)
        }
        expr => {
            let prim = match k_mut {
                KMut::Const => KPrim::Ref,
                KMut::Mut => KPrim::RefMut,
            };
            emit_unary_op(prim, Some(expr), loc, gx)
        }
    }
}

fn gen_expr(expr: &PExpr, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Number(PNumberExpr { token }) => gen_number_lit(*token, gx),
        PExpr::Char(PCharExpr { token }) => {
            let (text, loc) = token.decompose(&gx.tokens);
            KTerm::Char {
                text,
                ty: KTy2::C8,
                loc,
            }
        }
        PExpr::Str(PStrExpr { token }) => {
            let (text, loc) = token.decompose(&gx.tokens);
            KTerm::Str { text, loc }
        }
        PExpr::True(PTrueExpr { token }) => KTerm::True {
            loc: token.loc(&gx.tokens),
        },
        PExpr::False(PFalseExpr { token }) => KTerm::False {
            loc: token.loc(&gx.tokens),
        },
        PExpr::Name(name) => {
            let loc = name.of(&gx.names).token.of(&gx.tokens).loc();
            match gen_name(*name, gx) {
                KSymbolExt::Unresolved => {
                    error!("unresolved name {:?}", (name, loc));
                    new_unit_term(loc)
                }
                KSymbolExt::Alias { alias, loc } => KTerm::Alias { alias, loc },
                KSymbolExt::Symbol(symbol) => KTerm::Name(symbol),
                KSymbolExt::Const(k_const) => KTerm::Const { k_const, loc },
                KSymbolExt::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
                KSymbolExt::Fn(k_fn) => KTerm::Fn { k_fn, loc },
                KSymbolExt::ExternFn(extern_fn) => KTerm::ExternFn { extern_fn, loc },
                KSymbolExt::UnitLikeStruct { k_struct, loc } => {
                    let ty = KTy::Struct(k_struct);
                    let name = k_struct.name(&gx.mod_outline.structs).to_string();
                    let result = gx.fresh_symbol(&name, loc);
                    gx.push(KCommand::Node {
                        prim: KPrim::Record,
                        tys: vec![ty],
                        args: vec![],
                        result_opt: Some(result.clone()),
                        cont_count: 1,
                        loc,
                    });
                    KTerm::Name(result)
                }
            }
        }
        PExpr::Record(PRecordExpr {
            name,
            left_brace,
            fields,
            ..
        }) => {
            let k_struct = match *name.of(&gx.name_res) {
                NName::Const(_) => todo!(),
                NName::Struct(n_struct) => n_struct,
                n_name => unreachable!("{:?}", n_name),
            };

            let (name, loc) = (name.text(&gx.names).to_string(), name.loc());
            let result = gx.fresh_symbol(&name, loc);

            let field_count = k_struct.fields(&gx.mod_outline.structs).len();
            let mut args = vec![KTerm::Unit { loc }; field_count];
            let mut arg_freq = vec![0_u8; field_count];

            for field in fields {
                let term = gen_expr(field.value_opt.as_ref().unwrap(), gx);

                match (0..field_count).find(|&i| {
                    field.name.text(&gx.names).to_string()
                        == k_struct.fields(&gx.mod_outline.structs)[i].name(&gx.mod_outline.fields)
                }) {
                    Some(i) => {
                        arg_freq[i] += 1;
                        args[i] = term;
                    }
                    None => error_token(
                        field.name.of(&gx.names).token,
                        "このフィールドは構造体に属していません",
                        gx,
                    ),
                }
            }

            // フィールドへの割り当ての過不足を計算する。
            let mut missed = vec![];
            let mut duped = vec![];
            for (freq, k_field) in arg_freq
                .iter()
                .zip(k_struct.fields(&gx.mod_outline.structs))
            {
                match freq {
                    0 => missed.push(k_field.name(&gx.mod_outline.fields)),
                    1 => {}
                    _ => duped.push(k_field.name(&gx.mod_outline.fields)),
                }
            }

            if !duped.is_empty() {
                for field_expr in fields {
                    if duped.contains(&field_expr.name.text(&gx.names)) {
                        error_token(
                            field_expr.name.of(&gx.names).token,
                            "フィールドへの割り当てが重複しています",
                            gx,
                        );
                    }
                }
            }

            if !missed.is_empty() {
                error_token(
                    *left_brace,
                    format!(
                        "フィールドへの割り当てが不足しています: '{}'",
                        missed.join("', '")
                    ),
                    gx,
                );
            }

            gx.push(KCommand::Node {
                prim: KPrim::Record,
                tys: vec![KTy::Struct(k_struct)],
                args,
                result_opt: Some(result.clone()),
                cont_count: 1,
                loc,
            });

            KTerm::Name(result)
        }
        PExpr::Tuple(PTupleExpr { arg_list }) => match arg_list.args.as_slice() {
            [] => new_unit_term(arg_list.loc()),
            [arg] if !arg_list.is_tuple() => gen_expr(&arg.expr, gx),
            _ => {
                log::error!("タプルリテラルは未実装です {:?}", expr);
                new_never_term(arg_list.loc())
            }
        },
        PExpr::DotField(..) => {
            let (text, loc) = match expr {
                PExpr::DotField(PDotFieldExpr {
                    name_opt: Some(name),
                    ..
                }) => (name.text(&gx.tokens).to_string(), name.loc(&gx.tokens)),
                _ => unreachable!(),
            };

            let result1 = gen_expr_lval(expr, KMut::Const, loc, gx);
            let result2 = gx.fresh_symbol(&text, loc);

            gx.push_prim_1(KPrim::Deref, vec![result1], result2.clone());

            KTerm::Name(result2)
        }
        PExpr::Call(PCallExpr { left, arg_list }) => {
            let loc = arg_list.left_paren.loc(&gx.tokens);
            let result = gx.fresh_symbol("call_result", loc);

            let k_left = gen_expr(&left, gx);

            let mut k_args = vec![k_left];
            for p_arg in &arg_list.args {
                let k_arg = gen_expr(&p_arg.expr, gx);
                k_args.push(k_arg);
            }

            gx.push_prim_1(KPrim::CallDirect, k_args, result.clone());

            KTerm::Name(result)
        }
        PExpr::Index(PIndexExpr { .. }) => {
            let loc = expr.loc();
            let result = gx.fresh_symbol("index_result", loc);

            let indexed_ptr = gen_expr_lval(expr, KMut::Const, expr.loc(), gx);
            gx.push_prim_1(KPrim::Deref, vec![indexed_ptr], result.clone());
            KTerm::Name(result)
        }
        PExpr::As(PAsExpr {
            left,
            keyword,
            ty_opt,
            ..
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let result = gx.fresh_symbol("cast", loc);

            let left = gen_expr(&left, gx);
            let ty = gen_ty(ty_opt.as_ref().unwrap(), &gx.name_res);

            gx.push(KCommand::Node {
                prim: KPrim::Cast,
                tys: vec![ty],
                args: vec![left],
                result_opt: Some(result.clone()),
                cont_count: 1,
                loc,
            });

            KTerm::Name(result)
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            op,
            op_token,
            mut_opt,
            arg_opt,
        }) => {
            let loc = op_token.loc(&gx.tokens);
            match op {
                PUnaryOp::Deref => emit_unary_op(KPrim::Deref, arg_opt.as_deref(), loc, gx),
                PUnaryOp::Ref => {
                    let k_mut = gen_mut(mut_opt.as_ref());
                    gen_expr_lval(arg_opt.as_ref().unwrap(), k_mut, loc, gx)
                }
                PUnaryOp::Minus => emit_unary_op(KPrim::Minus, arg_opt.as_deref(), loc, gx),
                PUnaryOp::Not => emit_unary_op(KPrim::Not, arg_opt.as_deref(), loc, gx),
            }
        }
        PExpr::BinaryOp(PBinaryOpExpr {
            op,
            op_token,
            left,
            right_opt,
        }) => {
            let loc = op_token.loc(&gx.tokens);
            match op {
                PBinaryOp::Assign => {
                    let left = gen_expr_lval(&left, KMut::Mut, loc, gx);
                    let right = gen_expr(right_opt.as_ref().unwrap(), gx);

                    gx.push(KCommand::Node {
                        prim: KPrim::Assign,
                        tys: vec![],
                        args: vec![left, right],
                        result_opt: None,
                        cont_count: 1,
                        loc,
                    });

                    new_unit_term(loc)
                }
                PBinaryOp::AddAssign => {
                    emit_compound_assign(KPrim::AddAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::SubAssign => {
                    emit_compound_assign(KPrim::SubAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::MulAssign => {
                    emit_compound_assign(KPrim::MulAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::DivAssign => {
                    emit_compound_assign(KPrim::DivAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::ModuloAssign => {
                    emit_compound_assign(KPrim::ModuloAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::BitAndAssign => {
                    emit_compound_assign(KPrim::BitAndAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::BitOrAssign => {
                    emit_compound_assign(KPrim::BitOrAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::BitXorAssign => {
                    emit_compound_assign(KPrim::BitXorAssign, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::LeftShiftAssign => emit_compound_assign(
                    KPrim::LeftShiftAssign,
                    left,
                    right_opt.as_deref(),
                    loc,
                    gx,
                ),
                PBinaryOp::RightShiftAssign => emit_compound_assign(
                    KPrim::RightShiftAssign,
                    left,
                    right_opt.as_deref(),
                    loc,
                    gx,
                ),
                PBinaryOp::Add => emit_binary_op(KPrim::Add, left, right_opt.as_deref(), loc, gx),
                PBinaryOp::Sub => emit_binary_op(KPrim::Sub, left, right_opt.as_deref(), loc, gx),
                PBinaryOp::Mul => emit_binary_op(KPrim::Mul, left, right_opt.as_deref(), loc, gx),
                PBinaryOp::Div => emit_binary_op(KPrim::Div, left, right_opt.as_deref(), loc, gx),
                PBinaryOp::Modulo => {
                    emit_binary_op(KPrim::Modulo, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::BitAnd => {
                    emit_binary_op(KPrim::BitAnd, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::BitOr => {
                    emit_binary_op(KPrim::BitOr, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::BitXor => {
                    emit_binary_op(KPrim::BitXor, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::LeftShift => {
                    emit_binary_op(KPrim::LeftShift, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::RightShift => {
                    emit_binary_op(KPrim::RightShift, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::Equal => {
                    emit_binary_op(KPrim::Equal, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::NotEqual => {
                    emit_binary_op(KPrim::NotEqual, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::LessThan => {
                    emit_binary_op(KPrim::LessThan, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::LessEqual => {
                    emit_binary_op(KPrim::LessEqual, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::GreaterThan => {
                    emit_binary_op(KPrim::GreaterThan, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::GreaterEqual => {
                    emit_binary_op(KPrim::GreaterEqual, left, right_opt.as_deref(), loc, gx)
                }
                PBinaryOp::LogAnd => {
                    let false_term = new_false_term(loc);
                    emit_if(
                        &left,
                        |gx| gen_expr(right_opt.as_deref().unwrap(), gx),
                        move |_| false_term,
                        loc,
                        gx,
                    )
                }
                PBinaryOp::LogOr => {
                    let true_term = new_true_term(loc);
                    emit_if(
                        &left,
                        move |_| true_term,
                        |gx| gen_expr(right_opt.as_deref().unwrap(), gx),
                        loc,
                        gx,
                    )
                }
            }
        }
        PExpr::Pipe(PPipeExpr {
            left: arg,
            pipe,
            right_opt,
        }) => match right_opt.as_deref() {
            Some(PExpr::Call(PCallExpr { left, arg_list })) => {
                // FIXME: call expr の生成と共通化
                let result = {
                    let loc = pipe.loc(&gx.tokens);
                    gx.fresh_symbol("call_result", loc)
                };

                let k_arg = gen_expr(&arg, gx);
                let k_left = gen_expr(&left, gx);

                let mut k_args = vec![k_left, k_arg];
                for p_arg in &arg_list.args {
                    let k_arg = gen_expr(&p_arg.expr, gx);
                    k_args.push(k_arg);
                }

                gx.push_prim_1(KPrim::CallDirect, k_args, result.clone());
                KTerm::Name(result)
            }
            _ => {
                let loc = pipe.loc(&gx.tokens);
                error_token(*pipe, "|> の右辺は関数呼び出しでなければいけません", gx);
                new_never_term(loc)
            }
        },
        PExpr::Block(PBlockExpr(block)) => gen_block(block, gx),
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt,
            loop_id_opt,
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let n_loop = NLoop::from_index(loop_id_opt.unwrap());

            let label = gx.current_loops[n_loop].break_label;
            let arg = match arg_opt {
                Some(arg) => gen_expr(&arg, gx),
                None => new_unit_term(loc),
            };
            gx.push_jump_with_cont(label, vec![arg], loc);

            new_never_term(loc)
        }
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt,
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let n_loop = NLoop::from_index(loop_id_opt.unwrap());

            let label = gx.current_loops[n_loop].continue_label;
            gx.push_jump_with_cont(label, vec![], loc);

            new_never_term(loc)
        }
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt,
            fn_id_opt,
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let k_fn = KFn::from_index(fn_id_opt.unwrap());

            let args = {
                let return_term = KTerm::Return { k_fn, loc };
                let arg = match arg_opt {
                    Some(arg) => gen_expr(&arg, gx),
                    None => new_unit_term(loc),
                };
                vec![return_term, arg]
            };
            gx.push(KCommand::Node {
                prim: KPrim::Jump,
                tys: vec![],
                args,
                result_opt: None,
                cont_count: 1,
                loc,
            });

            new_never_term(loc)
        }
        PExpr::If(PIfExpr {
            keyword,
            cond_opt,
            body_opt,
            alt_opt,
            ..
        }) => {
            let loc = keyword.loc(&gx.tokens);
            emit_if(
                &cond_opt.as_ref().unwrap(),
                |gx| gen_block(body_opt.as_ref().unwrap(), gx),
                move |gx| match alt_opt {
                    Some(alt) => gen_expr(&alt, gx),
                    None => new_unit_term(loc),
                },
                loc,
                gx,
            )
        }
        PExpr::Match(PMatchExpr {
            keyword,
            cond_opt,
            arms,
            ..
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let k_cond = gen_expr(cond_opt.as_ref().unwrap(), gx);
            if arms.is_empty() {
                return new_never_term(loc).clone();
            }

            let result = gx.fresh_symbol("match_result", loc);
            let next_label = gx.fresh_label("match_next", loc);

            // 比較される値を計算する。
            let args = once(k_cond.clone())
                .chain(arms.iter().map(|arm| match &arm.pat {
                    PPat::Char(token) => {
                        let (text, loc) = token.decompose(&gx.tokens);
                        KTerm::Char {
                            text,
                            ty: KTy2::C8,
                            loc,
                        }
                    }
                    PPat::Name(name) => {
                        let loc = name.loc();
                        match *name.of(&gx.name_res) {
                            NName::Const(k_const) => KTerm::Const { k_const, loc },
                            NName::LocalVar(_) => match gen_name(*name, gx).as_symbol() {
                                Some(symbol) => KTerm::Name(symbol),
                                None => new_never_term(loc),
                            },
                            _ => {
                                error!("unimplemented pat {:?}", arm);
                                new_never_term(loc)
                            }
                        }
                    }
                    PPat::Record(PRecordPat { name, .. }) => {
                        let loc = name.loc();
                        let k_struct = name.of(&gx.name_res).as_struct().unwrap();
                        KTerm::RecordTag { k_struct, loc }
                    }
                }))
                .collect();
            let cont_count = arms.len();

            gx.push(KCommand::Node {
                prim: KPrim::Switch,
                tys: vec![],
                args,
                result_opt: None,
                cont_count,
                loc,
            });

            // 分岐後の処理を生成する。
            for arm in arms {
                match &arm.pat {
                    PPat::Char(_) => {}
                    PPat::Name(name) => match *name.of(&gx.name_res) {
                        NName::Const(_) => {}
                        NName::LocalVar(_) => {
                            // パターン変数に値を代入する。
                            if let Some(name) = gen_name(*name, gx).as_symbol() {
                                gx.push_prim_1(KPrim::Let, vec![k_cond.clone()], name);
                            }
                        }
                        _ => error!("unimplemented pat {:?}", name),
                    },
                    PPat::Record(_) => {}
                }

                let body = gen_expr(arm.body_opt.as_deref().unwrap(), gx);
                gx.push_jump(next_label, vec![body], arm.loc());
            }

            gx.push_label(next_label, vec![result.clone()]);
            KTerm::Name(result)
        }
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let result = gx.fresh_symbol("while_result", loc);
            let unit_term = new_unit_term(loc);

            let KLoopData {
                break_label: next_label,
                continue_label,
            } = {
                let n_loop = NLoop::from_index(loop_id_opt.unwrap());
                let loop_data = fresh_loop_labels(loc, gx);
                gx.current_loops[n_loop] = loop_data.clone();
                loop_data
            };

            gx.push_jump(continue_label, vec![], loc);

            gx.push_label(continue_label, vec![]);

            let k_cond = gen_expr(cond_opt.as_ref().unwrap(), gx);

            gx.push(KCommand::Node {
                prim: KPrim::If,
                tys: vec![],
                args: vec![k_cond],
                result_opt: None,
                cont_count: 2,
                loc,
            });

            // body:
            gen_block(body_opt.as_ref().unwrap(), gx);

            gx.push_jump(continue_label, vec![], loc);

            // alt:
            gx.push_jump(next_label, vec![unit_term.clone()], loc);

            // next:
            gx.push_label(next_label, vec![result.clone()]);

            unit_term
        }
        PExpr::Loop(PLoopExpr {
            keyword,
            body_opt,
            loop_id_opt,
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let result = gx.fresh_symbol("loop_result", loc);

            let KLoopData {
                break_label: next_label,
                continue_label,
            } = {
                let n_loop = NLoop::from_index(loop_id_opt.unwrap());
                let loop_data = fresh_loop_labels(loc, gx);
                gx.current_loops[n_loop] = loop_data.clone();
                loop_data
            };

            gx.push_jump(continue_label.clone(), vec![], loc);

            gx.push_label(continue_label.clone(), vec![]);

            // body:
            gen_block(body_opt.as_ref().unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![], loc);

            // next:
            gx.push_label(next_label, vec![result.clone()]);

            KTerm::Name(result)
        }
    }
}

fn gen_decl(decl: &PDecl, gx: &mut Gx) {
    match decl {
        PDecl::Expr(PExprDecl { expr, .. }) => {
            gen_expr(expr, gx);
        }
        PDecl::Let(PLetDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            let ty = ty_opt
                .as_ref()
                .map_or(KTy::Unresolved, |ty| gen_ty(ty, &gx.name_res));

            let k_init = gen_expr(init_opt.as_ref().unwrap(), gx);
            let mut result = gen_name(name_opt.unwrap(), gx).as_symbol().unwrap();
            // FIXME: k_mod
            *result.ty_mut(&mut gx.current_locals) = ty.to_ty2(KMod::from_index(0));

            gx.push_prim_1(KPrim::Let, vec![k_init], result);
        }
        PDecl::Const(PConstDecl {
            name_opt, init_opt, ..
        }) => {
            let name = name_opt.clone().unwrap();
            let k_const = match *name.of(&gx.name_res) {
                NName::Const(n_const) => n_const,
                _ => unreachable!(),
            };
            assert_eq!(
                name.text(&gx.names),
                k_const.of(&gx.mod_outline.consts).name
            );

            let init = init_opt.as_ref().unwrap();
            let init_loc = init.loc();
            let value_opt = match gen_constant(init, gx) {
                Some(value) => Some(value),
                None => {
                    error_node(init_loc, "定数式として不正です", gx);
                    None
                }
            };

            k_const.of_mut(&mut gx.mod_outline.consts).value_opt = value_opt;
        }
        PDecl::Static(PStaticDecl {
            name_opt, init_opt, ..
        }) => {
            let name = name_opt.clone().unwrap();
            let static_var = match *name.of(&gx.name_res) {
                NName::StaticVar(n_static_var) => n_static_var,
                _ => unreachable!(),
            };
            assert_eq!(
                name.text(&gx.names),
                static_var.name(&gx.mod_outline.static_vars)
            );

            let init = init_opt.as_ref().unwrap();
            let init_loc = init.loc();
            let value_opt = match gen_constant(init, gx) {
                Some(value) => Some(value),
                None => {
                    error_node(init_loc, "定数式として不正です", gx);
                    None
                }
            };

            static_var.of_mut(&mut gx.mod_outline.static_vars).value_opt = value_opt;
        }
        PDecl::Fn(PFnDecl {
            keyword,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let loc = keyword.loc(&gx.tokens);
            let k_fn = KFn::from_index(fn_id_opt.unwrap());

            let parent_local_vars = {
                let local_vars = take(&mut gx.fns[k_fn].locals);
                replace(&mut gx.current_locals, local_vars)
            };

            let (commands, labels) = {
                let parent_commands = take(&mut gx.current_commands);
                let parent_labels = take(&mut gx.current_labels);
                let parent_loops = take(&mut gx.current_loops);

                // ここでラベルを生成すると順番が構文と食い違うので、ループの宣言を見たときに生成する。
                gx.current_loops = VecArena::from_vec(
                    take(&mut gx.fn_loops[k_fn])
                        .iter()
                        .map(|_| KLoopData {
                            break_label: KLabel::MAX,
                            continue_label: KLabel::MAX,
                        })
                        .collect(),
                );

                let result = gen_block(block_opt.as_ref().unwrap(), gx);
                gx.push(KCommand::Node {
                    prim: KPrim::Jump,
                    tys: vec![],
                    args: vec![KTerm::Return { k_fn, loc }, result],
                    result_opt: None,
                    cont_count: 1,
                    loc,
                });

                let commands = replace(&mut gx.current_commands, parent_commands);
                let labels = replace(&mut gx.current_labels, parent_labels);
                gx.current_loops = parent_loops;
                (commands, labels)
            };

            let (body, labels) = fold_block(commands, labels);

            let locals = replace(&mut gx.current_locals, parent_local_vars);

            let mut data = &mut gx.fns[k_fn];
            data.body = body;
            data.locals = locals;
            data.labels = labels;
        }
        PDecl::ExternFn(_) => {}
        PDecl::Enum(PEnumDecl {
            name_opt, variants, ..
        }) => {
            let name = name_opt.unwrap();
            let k_enum = match *name.of(&gx.name_res) {
                NName::Enum(n_enum) => n_enum,
                n_name_opt => unreachable!("{:?}", n_name_opt),
            };
            assert_eq!(name.text(&gx.names), k_enum.name(&gx.mod_outline.enums));

            let mut next_value = 0_usize;
            let _k_variants = variants
                .into_iter()
                .map(|variant_decl| {
                    let k_variant = gen_variant(variant_decl, &mut next_value, gx);
                    next_value += 1;
                    k_variant
                })
                .collect::<Vec<_>>();
        }
        PDecl::Struct(PStructDecl { variant_opt, .. }) => {
            if let Some(variant) = variant_opt {
                gen_variant(variant, &mut 0, gx);
            }
        }
        PDecl::Use(_) => {}
    }
}

fn gen_block(block: &PBlock, gx: &mut Gx) -> KTerm {
    for decl in &block.decls {
        gen_decl(decl, gx);
    }

    match &block.last_opt {
        Some(last) => gen_expr(last, gx),
        None => {
            let loc = block.left_brace.loc(&gx.tokens);
            new_unit_term(loc)
        }
    }
}

fn gen_root(root: &PRoot, gx: &mut Gx) {
    for decl in &root.decls {
        gen_decl(decl, gx);
    }
}

pub(crate) fn cps_conversion(
    k_mod: KMod,
    p_root: &PRoot,
    name_resolution: &NameResolution,
    logger: DocLogger,
) -> (KModOutline, KModData) {
    let mod_info = {
        let k_consts = name_resolution
            .consts
            .iter()
            .map(|n_const_data| KConstData {
                name: n_const_data.name.to_string(),
                value_ty: n_const_data.value_ty.clone(),
                value_opt: {
                    // この段階では定数式は計算できない。
                    None
                },
                parent_opt: n_const_data.parent_opt,
                loc: n_const_data.loc,
            })
            .collect();

        let static_vars = name_resolution
            .static_vars
            .iter()
            .map(|n_static_var_data| KStaticVarData {
                name: n_static_var_data.name.to_string(),
                ty: n_static_var_data.ty.clone(),
                value_opt: {
                    // この段階では定数式は計算できない。
                    None
                },
                loc: n_static_var_data.loc,
            })
            .collect();

        // 関数の ID, 名前ID の対応表を構築する。
        let n_fns = &name_resolution.fns;
        let fn_loops = n_fns
            .iter()
            .map(|fn_data| fn_data.loops.clone())
            .collect::<Vec<_>>();
        let fn_outlines = n_fns
            .iter()
            .map(|data| KFnOutline {
                name: data.name.to_string(),
                vis_opt: data.vis_opt,
                param_tys: data
                    .params
                    .iter()
                    .map(|symbol| symbol.local.of(&data.local_vars).ty.clone())
                    .collect(),
                result_ty: data.result_ty.clone(),
                loc: data.loc,
            })
            .collect();
        let fns = n_fns
            .iter()
            .map(|fn_data| KFnData {
                params: fn_data.params.clone(),
                locals: KLocalArena::from_vec(
                    fn_data
                        .local_vars
                        .iter()
                        .map(|n_local_var_data| {
                            KLocalData::new(n_local_var_data.name.to_string(), n_local_var_data.loc)
                                .with_ty(n_local_var_data.ty.to_ty2(k_mod))
                        })
                        .collect(),
                ),
                body: Default::default(),
                labels: Default::default(),
                label_sigs: Default::default(),
                ty_env: Default::default(),
            })
            .collect();

        // 外部関数の ID, 名前ID の対応表を構築する。
        let n_extern_fns = &name_resolution.extern_fns;
        let extern_fn_outlines = n_extern_fns
            .iter()
            .map(|data| KExternFnOutline {
                name: data.name.to_string(),
                param_tys: data
                    .params
                    .iter()
                    .map(|symbol| symbol.local.of(&data.local_vars).ty.clone())
                    .collect(),
                result_ty: data.result_ty.clone(),
                loc: data.loc,
            })
            .collect();
        let k_extern_fns = n_extern_fns
            .iter()
            .map(|extern_fn_data| KExternFnData {
                params: extern_fn_data.params.clone(),
                locals: KLocalArena::from_vec(
                    extern_fn_data
                        .local_vars
                        .iter()
                        .map(|n_local_var_data| {
                            KLocalData::new(n_local_var_data.name.to_string(), n_local_var_data.loc)
                                .with_ty(n_local_var_data.ty.to_ty2(k_mod))
                        })
                        .collect(),
                ),
            })
            .collect();

        let k_enums = name_resolution
            .enums
            .iter()
            .map(|n_enum_data| KEnumOutline {
                name: n_enum_data.name.to_string(),
                variants: n_enum_data.variants.clone(),
                loc: n_enum_data.loc,
            })
            .collect();

        let k_structs = name_resolution
            .structs
            .iter()
            .map(|n_struct_data| KStructOutline {
                name: n_struct_data.name.to_string(),
                fields: n_struct_data.fields.clone(),
                parent_opt: n_struct_data.parent_opt.map(KStructParent::new),
                loc: n_struct_data.loc,
            })
            .collect();

        let k_fields = name_resolution
            .fields
            .iter()
            .map(|n_field_data| KFieldOutline {
                name: n_field_data.name.to_string(),
                ty: n_field_data.ty.clone(),
                loc: n_field_data.loc,
            })
            .collect();

        let mut gx = Gx::new(
            &p_root.tokens,
            &p_root.names,
            &name_resolution.names,
            logger,
        );

        gx.mod_outline.aliases = name_resolution.aliases.clone();

        gx.mod_outline.consts = VecArena::from_vec(k_consts);

        gx.mod_outline.static_vars = VecArena::from_vec(static_vars);

        gx.fns = VecArena::from_vec(fns);
        gx.fn_loops = VecArena::from_vec(fn_loops);
        gx.mod_outline.fns = VecArena::from_vec(fn_outlines);

        gx.extern_fns = VecArena::from_vec(k_extern_fns);
        gx.mod_outline.extern_fns = VecArena::from_vec(extern_fn_outlines);

        gx.mod_outline.enums = VecArena::from_vec(k_enums);

        gx.mod_outline.structs = VecArena::from_vec(k_structs);

        gx.mod_outline.fields = VecArena::from_vec(k_fields);

        gen_root(p_root, &mut gx);

        (
            gx.mod_outline,
            KModData {
                extern_fns: gx.extern_fns,
                fns: gx.fns,
            },
        )
    };

    trace!("k_root (untyped) = {:#?}\n", mod_info);

    mod_info
}

// =============================================================================
// V2
// =============================================================================

/// CPS 変換の文脈
struct Xx<'a> {
    env: Env,
    nodes: Vec<KNode>,
    local_vars: KLocalArena,
    mod_data: KModData,
    doc: Doc,
    k_mod: KMod,
    tokens: &'a PTokens,
    root: &'a PRoot,
    ast: &'a ATree,
    decl_symbols: &'a DeclSymbols,
    mod_outline: &'a KModOutline,
    logger: &'a DocLogger,
}

impl<'a> Xx<'a> {
    fn new(
        doc: Doc,
        k_mod: KMod,
        root: &'a PRoot,
        decl_symbols: &'a DeclSymbols,
        mod_outline: &'a KModOutline,
        logger: &'a DocLogger,
    ) -> Self {
        Self {
            env: Env::new(),
            nodes: vec![],
            local_vars: KLocalArena::new(),
            mod_data: KModData::default(),
            doc,
            k_mod,
            tokens: &root.tokens,
            root,
            ast: &root.ast,
            decl_symbols,
            mod_outline,
            logger,
        }
    }

    fn do_in_scope(&mut self, f: impl FnOnce(&mut Xx)) {
        self.env.enter_scope();
        f(self);
        self.env.leave_scope();
    }
}

fn error_unresolved_ty(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これは型の名前だと思いますが、定義が見つかりません。")
}

fn error_unresolved_value(loc: PLoc, logger: &DocLogger) {
    logger.error(loc, "これは値の名前だと思いますが、定義が見つかりません。");
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) struct TyResolver<'a> {
    pub(crate) env: &'a Env,
    pub(crate) root: &'a PRoot,
    pub(crate) ast: &'a ATree,
    pub(crate) logger: &'a DocLogger,
}

fn new_ty_resolver<'a>(xx: &'a Xx<'a>) -> TyResolver<'a> {
    TyResolver {
        env: &xx.env,
        root: xx.root,
        ast: xx.ast,
        logger: xx.logger,
    }
}

fn do_convert_ty(ty_id: ATyId, ty: &ATy, xx: &TyResolver) -> KTy {
    match ty {
        ATy::Name(AName { text, .. }) => match resolve_ty_name2(&text, xx.env) {
            Some(ty) => ty,
            None => {
                error_unresolved_ty(ty_id.loc(xx.root), xx.logger);
                KTy::Unresolved
            }
        },
        ATy::Never => KTy::Never,
        ATy::Unit => KTy::Unit,
        ATy::Ptr(APtrTy { mut_opt, ty_opt }) => {
            let k_mut = mut_opt.unwrap_or(KMut::Const);
            let base_ty = convert_ty_opt(*ty_opt, xx);
            base_ty.into_ptr(k_mut)
        }
    }
}

fn convert_ty(ty_id: ATyId, xx: &TyResolver) -> KTy {
    let ty = ty_id.of(xx.ast.tys());
    do_convert_ty(ty_id, ty, xx)
}

pub(crate) fn convert_ty_opt(ty_opt: Option<ATyId>, xx: &TyResolver) -> KTy {
    ty_opt.map_or(KTy::Unresolved, |ty| convert_ty(ty, xx))
}

// -----------------------------------------------
// 式 (項とノード)
// -----------------------------------------------

fn new_error_term(loc: Loc) -> KTerm {
    KTerm::Unit { loc }
}

fn new_cont() -> KNode {
    KNode::default()
}

fn convert_char_expr(token: PToken, tokens: &PTokens) -> KTerm {
    KTerm::Char {
        text: token.text(tokens).to_string(),
        ty: KTy2::C8,
        loc: token.loc(tokens),
    }
}

fn convert_str_expr(token: PToken, tokens: &PTokens) -> KTerm {
    KTerm::Str {
        text: token.text(tokens).to_string(),
        loc: token.loc(tokens),
    }
}

fn emit_unit_like_struct(
    k_struct: KStruct,
    result: KSymbol,
    loc: Loc,
    nodes: &mut Vec<KNode>,
) -> KTerm {
    let ty = KTy::Struct(k_struct);

    nodes.push(new_record_node(ty, result, new_cont(), loc));
    KTerm::Name(result)
}

fn fresh_symbol(hint: &str, loc: Loc, xx: &mut Xx) -> KSymbol {
    let local = xx.local_vars.alloc(KLocalData::new(hint.to_string(), loc));
    KSymbol { local, loc }
}

fn convert_name_expr(name: &str, loc: Loc, xx: &mut Xx) -> KTerm {
    let value = match resolve_value_name(name, loc, &mut xx.env) {
        Some(value) => value,
        None => {
            error_unresolved_value(PLoc::from_loc(loc), xx.logger);
            return new_error_term(loc);
        }
    };

    match value {
        KLocalValue::LocalVar(local_var) => KTerm::Name(KSymbol {
            local: local_var,
            loc,
        }),
        KLocalValue::Const(k_const) => KTerm::Const { k_const, loc },
        KLocalValue::StaticVar(static_var) => KTerm::StaticVar { static_var, loc },
        KLocalValue::Fn(k_fn) => KTerm::Fn { k_fn, loc },
        KLocalValue::ExternFn(extern_fn) => KTerm::ExternFn { extern_fn, loc },
        KLocalValue::UnitLikeStruct(k_struct) => {
            let name = k_struct.name(&xx.mod_outline.structs);
            let result = fresh_symbol(name, loc, xx);
            emit_unit_like_struct(k_struct, result, loc, &mut xx.nodes)
        }
        KLocalValue::Alias(alias) => KTerm::Alias { alias, loc },
    }
}

fn convert_block_expr(decls: ADeclIds, loc: Loc, xx: &mut Xx) -> KTerm {
    let mut last_opt = None;

    xx.do_in_scope(|xx| {
        last_opt = convert_decls(decls.clone(), xx);
    });

    last_opt.unwrap_or(KTerm::Unit { loc })
}

fn do_convert_expr(expr_id: AExprId, expr: &AExpr, xx: &mut Xx) -> KTerm {
    let loc = expr_id.loc(xx.root).to_loc(xx.doc);

    match expr {
        AExpr::Number(token) => convert_number_lit(*token, xx.tokens, xx.logger),
        AExpr::Char(token) => convert_char_expr(*token, xx.tokens),
        AExpr::Str(token) => convert_str_expr(*token, xx.tokens),
        AExpr::True => KTerm::True { loc },
        AExpr::False => KTerm::False { loc },
        AExpr::Name(AName { full_name, .. }) => convert_name_expr(full_name, loc, xx),
        AExpr::Unit => KTerm::Unit { loc },
        AExpr::Record(_) => todo!(),
        AExpr::DotField(_) => todo!(),
        AExpr::Call(_) => todo!(),
        AExpr::Index(_) => todo!(),
        AExpr::As(_) => todo!(),
        AExpr::UnaryOp(_) => todo!(),
        AExpr::BinaryOp(_) => todo!(),
        AExpr::Pipe(_) => todo!(),
        AExpr::Block(ABlockExpr { decls }) => convert_block_expr(decls.clone(), loc, xx),
        AExpr::Break(_) => todo!(),
        AExpr::Continue => todo!(),
        AExpr::Return(_) => todo!(),
        AExpr::If(_) => todo!(),
        AExpr::Match(_) => todo!(),
        AExpr::While(_) => todo!(),
        AExpr::Loop(_) => todo!(),
    }
}

fn convert_expr(expr_id: AExprId, xx: &mut Xx) -> KTerm {
    let expr = expr_id.of(xx.ast.exprs());
    do_convert_expr(expr_id, expr, xx)
}

fn convert_expr_opt(expr_id_opt: Option<AExprId>, loc: Loc, xx: &mut Xx) -> KTerm {
    match expr_id_opt {
        Some(expr_id) => convert_expr(expr_id, xx),
        None => new_error_term(loc),
    }
}

fn convert_let_decl(local_var: KLocal, decl: &AFieldLikeDecl, loc: Loc, xx: &mut Xx) {
    let value = convert_expr_opt(decl.value_opt, loc, xx);
    let ty = convert_ty_opt(decl.ty_opt, &new_ty_resolver(xx));

    local_var.of_mut(&mut xx.local_vars).ty = ty.to_ty2(xx.k_mod);
}

fn convert_extern_fn_decl(
    extern_fn: KExternFn,
    extern_fn_decl: &AFnLikeDecl,
    loc: Loc,
    xx: &mut Xx,
) {
    let mut locals = KLocalArena::new();

    let params = extern_fn_decl
        .params
        .iter()
        .zip(extern_fn.param_tys(&xx.mod_outline.extern_fns))
        .map(|(param_decl, param_ty)| {
            // FIXME: param_decl のロケーションを取る
            let name = param_decl.name.text.to_string();
            let local = locals.alloc(KLocalData::new(name, loc));
            KSymbol { local, loc }
        })
        .collect();

    *extern_fn.of_mut(&mut xx.mod_data.extern_fns) = KExternFnData { params, locals };
}

fn do_convert_decl(decl_id: ADeclId, decl: &ADecl, term_opt: &mut Option<KTerm>, xx: &mut Xx) {
    let symbol_opt = *decl_id.of(xx.decl_symbols);
    let loc = decl_id.loc(xx.root).to_loc(xx.doc);

    match decl {
        ADecl::Expr(expr) => {
            let term = convert_expr(*expr, xx);
            *term_opt = Some(term);
        }
        ADecl::Let(decl) => {
            let local_var = match symbol_opt {
                Some(KModLocalSymbol::LocalVar { local_var, .. }) => local_var,
                _ => return,
            };
            convert_let_decl(local_var, decl, loc, xx);
        }
        ADecl::Const(AFieldLikeDecl {
            modifiers: _,
            name_opt,
            ty_opt,
            value_opt,
        }) => todo!(),
        ADecl::Static(_) => todo!(),
        ADecl::Fn(_) => todo!(),
        ADecl::ExternFn(extern_fn_decl) => {
            let extern_fn = match symbol_opt {
                Some(KModLocalSymbol::ExternFn(it)) => it,
                _ => return,
            };
            convert_extern_fn_decl(extern_fn, extern_fn_decl, loc, xx);
        }
        ADecl::Enum(_) => todo!(),
        ADecl::Struct(_) => todo!(),
        ADecl::Use(_) => {}
    }
}

fn convert_decls(decls: ADeclIds, xx: &mut Xx) -> Option<KTerm> {
    for (decl_id, decl) in decls.enumerate(xx.ast.decls()) {
        if decl_allows_forward_reference(decl) {
            add_decl_to_local_env(decl_id, decl, xx.decl_symbols, xx.mod_outline, &mut xx.env);
        }
    }

    let mut last_opt = None;
    for (decl_id, decl) in decls.enumerate(xx.ast.decls()) {
        let mut term_opt = None;

        do_convert_decl(decl_id, decl, &mut term_opt, xx);

        if !decl_allows_forward_reference(decl) {
            add_decl_to_local_env(decl_id, decl, xx.decl_symbols, xx.mod_outline, &mut xx.env);
        }
        last_opt = term_opt;
    }
    last_opt
}

pub(crate) fn convert_to_cps(
    doc: Doc,
    k_mod: KMod,
    root: &PRoot,
    decl_symbols: &DeclSymbols,
    mod_outline: &KModOutline,
    logger: &DocLogger,
) {
    let mut xx = Xx::new(doc, k_mod, root, decl_symbols, mod_outline, logger);

    xx.do_in_scope(|xx| {
        convert_decls(root.ast.root_decls(), xx);
    });
}
