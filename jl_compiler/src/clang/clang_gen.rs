//! CPS 中間表現をC言語のコードに変換する処理

use super::*;
use crate::utils::VecArena;
use c_stmt::CStorageModifier;
use std::{
    collections::HashMap,
    mem::{replace, take},
};

type IdentMap = HashMap<String, IdProvider>;

/// C code generation context.
struct Cx<'a> {
    outlines: &'a KModOutline,
    mod_outlines: &'a KModOutlines,
    ident_map: HashMap<String, IdProvider>,
    static_var_ident_ids: Vec<Option<usize>>,
    fn_ident_ids: Vec<Option<usize>>,
    enum_ident_ids: Vec<Option<usize>>,
    struct_ident_ids: Vec<Option<usize>>,
    locals: KLocalArena,
    local_ident_ids: Vec<Option<usize>>,
    label_raw_names: VecArena<KLabelTag, String>,
    label_param_lists: VecArena<KLabelTag, Vec<KSymbol>>,
    label_ident_ids: Vec<Option<usize>>,
    stmts: Vec<CStmt>,
    decls: Vec<CStmt>,
}

impl<'a> Cx<'a> {
    fn new(mod_outline: &'a KModOutline, mod_outlines: &'a KModOutlines) -> Self {
        Self {
            outlines: mod_outline,
            mod_outlines,
            ident_map: Default::default(),
            static_var_ident_ids: Default::default(),
            fn_ident_ids: Default::default(),
            enum_ident_ids: Default::default(),
            struct_ident_ids: Default::default(),
            locals: Default::default(),
            local_ident_ids: Default::default(),
            label_raw_names: Default::default(),
            label_param_lists: Default::default(),
            label_ident_ids: Default::default(),
            stmts: Default::default(),
            decls: Default::default(),
        }
    }

    fn enter_block(&mut self, gen_fn: impl FnOnce(&mut Self)) -> Vec<CStmt> {
        let stmts = take(&mut self.stmts);

        gen_fn(self);

        replace(&mut self.stmts, stmts)
    }
}

fn format_unique_name(raw_name: &str, ident_id: usize) -> String {
    format!("{}_{:x}", raw_name, ident_id)
}

fn do_unique_name(
    id: usize,
    raw_name: &str,
    ident_ids: &mut Vec<Option<usize>>,
    ident_map: &mut IdentMap,
) -> String {
    let ident_id = match ident_ids[id] {
        Some(ident_id) => ident_id,
        None => match ident_map.get_mut(raw_name) {
            Some(ids) => {
                let ident_id = ids.next();
                ident_ids[id] = Some(ident_id);
                ident_id
            }
            None => {
                let mut ids = IdProvider::default();
                let ident_id = ids.next();
                ident_ids[id] = Some(ident_id);
                ident_map.insert(raw_name.to_string(), ids);
                ident_id
            }
        },
    };
    format_unique_name(raw_name, ident_id)
}

fn unique_name(symbol: &KSymbol, cx: &mut Cx) -> String {
    unique_local_name(symbol.local, cx)
}

fn unique_static_var_name(static_var: KStaticVar, cx: &mut Cx) -> String {
    do_unique_name(
        static_var.to_index(),
        static_var.name(&cx.outlines.static_vars),
        &mut cx.static_var_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_fn_name(k_fn: KFn, cx: &mut Cx) -> String {
    // pub な関数の名前はマングルしない。
    if k_fn.is_pub(&cx.outlines.fns) {
        return k_fn.name(&cx.outlines.fns).to_string();
    }

    do_unique_name(
        k_fn.to_index(),
        k_fn.name(&cx.outlines.fns),
        &mut cx.fn_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_extern_fn_name(extern_fn: KExternFn, cx: &mut Cx) -> String {
    // 外部関数の名前は勝手にマングルしない。
    extern_fn.name(&cx.outlines.extern_fns).to_string()
}

fn unique_enum_name(k_enum: KEnum, cx: &mut Cx) -> String {
    do_unique_name(
        k_enum.to_index(),
        &cx.outlines.enums[k_enum].name,
        &mut cx.enum_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_struct_name(k_struct: KStruct, cx: &mut Cx) -> String {
    do_unique_name(
        k_struct.to_index(),
        &cx.outlines.structs[k_struct].name,
        &mut cx.struct_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_local_name(local: KLocal, cx: &mut Cx) -> String {
    do_unique_name(
        local.to_index(),
        &cx.locals[local].name,
        &mut cx.local_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_label_name(label: KLabel, cx: &mut Cx) -> String {
    do_unique_name(
        label.to_index(),
        &cx.label_raw_names[label],
        &mut cx.label_ident_ids,
        &mut cx.ident_map,
    )
}

fn emit_var_decl(symbol: &KSymbol, init_opt: Option<CExpr>, ty_env: &KTyEnv, cx: &mut Cx) {
    let is_alive = symbol.local.of(&cx.locals).is_alive;
    let (name, ty) = gen_param(symbol, ty_env, cx);

    // 不要な変数なら束縛しない。
    if !is_alive {
        cx.stmts
            .push(CStmt::Comment(format!("{} is killed.", name)));

        if let Some(expr) = init_opt {
            cx.stmts.push(CStmt::Expr(expr));
        }
        return;
    }

    cx.stmts.push(CStmt::VarDecl {
        storage_modifier_opt: None,
        name,
        ty,
        init_opt,
    });
}

fn gen_constant_value(value: &KConstValue) -> CExpr {
    match value {
        KConstValue::I32(value) => CExpr::IntLit(value.to_string()),
        KConstValue::I64(value) => CExpr::LongLongLit(value.to_string()),
        KConstValue::Usize(value) => CExpr::UnsignedLongLongLit(value.to_string()),
        KConstValue::F64(value) => CExpr::DoubleLit(value.to_string()),
        KConstValue::Bool(true) => CExpr::BoolLit("1"),
        KConstValue::Bool(false) => CExpr::BoolLit("0"),
    }
}

fn gen_invalid_constant_value() -> CExpr {
    CExpr::Other("/* invalid const */ 0")
}

fn gen_record_tag(k_struct: KStruct, structs: &KStructArena) -> CExpr {
    // OK: タグ値は決定済みのはず
    let tag = k_struct.tag_value_opt(structs).unwrap();
    gen_constant_value(&tag)
}

fn gen_ty(ty: &KTy, ty_env: &KTyEnv, cx: &mut Cx) -> CTy {
    // FIXME: 整数型は std::int32_t とかの方がよいかも
    match ty {
        KTy::Unresolved => {
            error!("Unexpected unresolved type {:?}", ty);
            CTy::Other("/* unresolved */ void")
        }
        KTy::Meta(meta) => match meta.try_unwrap(&ty_env) {
            Some(ty) => gen_ty(&ty.borrow().to_ty1(), ty_env, cx),
            None => {
                error!("Unexpected free type {:?}", meta);
                CTy::Other("/* free */ void")
            }
        },
        KTy::Never => {
            // FIXME: error!
            CTy::Other("/* never */ void")
        }
        KTy::Fn { .. } => {
            // FIXME: この時点で fn 型は除去されているべき
            error!("Unexpected fn type {:?}", ty);
            CTy::Other("/* fn */ void")
        }
        KTy::Unit => CTy::Void,
        KTy::I8 => CTy::SignedChar,
        KTy::I16 => CTy::Short,
        KTy::I32 | KTy::Bool => CTy::Int,
        KTy::I64 | KTy::Isize => CTy::LongLong,
        KTy::U8 | KTy::C8 => CTy::UnsignedChar,
        KTy::U16 | KTy::C16 => CTy::UnsignedShort,
        KTy::U32 | KTy::C32 => CTy::UnsignedInt,
        KTy::U64 | KTy::Usize => CTy::UnsignedLongLong,
        KTy::F32 => CTy::Float,
        KTy::F64 => CTy::Double,
        KTy::Ptr { k_mut, ty } => {
            let mut ty = gen_ty(&ty, ty_env, cx);
            if let KMut::Const = k_mut {
                ty = ty.into_const();
            }
            ty.into_ptr()
        }
        KTy::Alias(_) => {
            // FIXME: 実装
            CTy::Other("/* error: alias ty */")
        }
        KTy::Enum(k_enum) => match k_enum.repr(&cx.outlines.enums) {
            KEnumRepr::Unit => CTy::Other("/* unit-like enum */ void"),
            KEnumRepr::Never => CTy::Other("/* never enum */ void"),
            KEnumRepr::Const { value_ty } => gen_ty(value_ty, ty_env, cx),
            KEnumRepr::Sum { .. } => CTy::Enum(unique_enum_name(*k_enum, cx)),
        },
        KTy::Struct(k_struct) => CTy::Struct(unique_struct_name(*k_struct, cx)),
    }
}

fn gen_param(param: &KSymbol, ty_env: &KTyEnv, cx: &mut Cx) -> (String, CTy) {
    let name = unique_name(param, cx);
    let ty = param.ty(&cx.locals);
    (name, gen_ty(&ty.to_ty1(), &ty_env, cx))
}

fn gen_const_data(const_data: &KConstData) -> CExpr {
    match &const_data.value_opt {
        Some(value) => gen_constant_value(value),
        None => gen_invalid_constant_value(),
    }
}

fn gen_static_var_term(static_var: KStaticVar, cx: &mut Cx) -> CExpr {
    CExpr::Name(unique_static_var_name(static_var, cx))
}

fn gen_fn_term(k_fn: KFn, cx: &mut Cx) -> CExpr {
    CExpr::Name(unique_fn_name(k_fn, cx))
}

fn gen_extern_fn_term(extern_fn: KExternFn, cx: &mut Cx) -> CExpr {
    CExpr::Name(unique_extern_fn_name(extern_fn, cx))
}

fn gen_term(term: &KTerm, cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Unit { .. } => {
            // FIXME: error!
            CExpr::Other("(void)0")
        }
        KTerm::Int(token, KTy::I64) | KTerm::Int(token, KTy::Isize) => CExpr::LongLongLit(
            token
                .text()
                .replace("_", "")
                .replace("i64", "")
                .replace("isize", ""),
        ),
        KTerm::Int(token, KTy::U64) | KTerm::Int(token, KTy::Usize) => CExpr::UnsignedLongLongLit(
            token
                .text()
                .replace("_", "")
                .replace("u64", "")
                .replace("usize", ""),
        ),
        KTerm::Int(token, _) => CExpr::IntLit(token.text().replace("_", "").replace("i32", "")),
        KTerm::Float(token) => CExpr::DoubleLit(token.text().replace("_", "")),
        KTerm::Char(token) => CExpr::CharLit(token.text().to_string()),
        KTerm::Str(token) => CExpr::Cast {
            ty: CTy::UnsignedChar.into_const().into_ptr(),
            arg: Box::new(CExpr::StrLit(token.text().to_string())),
        },
        KTerm::True(_) => CExpr::BoolLit("1"),
        KTerm::False(_) => CExpr::BoolLit("0"),
        KTerm::Alias { alias, location } => match alias.of(&cx.outlines.aliases).referent() {
            Some(KProjectSymbol::ModLocal { k_mod, symbol }) => match symbol {
                KModLocalSymbol::Const(k_const) => {
                    gen_const_data(k_const.of(&k_mod.of(&cx.mod_outlines).consts))
                }
                KModLocalSymbol::StaticVar(static_var) => gen_static_var_term(static_var, cx),
                KModLocalSymbol::Fn(k_fn) => {
                    let fn_name = k_fn.of(&k_mod.of(&cx.mod_outlines).fns).name.to_string();
                    CExpr::Name(fn_name)
                }
                KModLocalSymbol::ExternFn(extern_fn) => {
                    // FIXME: fn と同様に k_mod の値を見るように修正
                    gen_extern_fn_term(extern_fn, cx)
                }
                KModLocalSymbol::LocalVar { .. }
                | KModLocalSymbol::Alias(_)
                | KModLocalSymbol::Enum(_)
                | KModLocalSymbol::Struct(_) => {
                    error!("別名の参照先が不正です {:?}", (symbol, location));
                    CExpr::Other("/* error: invalid alias term ")
                }
            },
            Some(KProjectSymbol::Mod(_)) => {
                error!("モジュールを指す別名はCの式になりません {:?}", location);
                CExpr::Other("/* error: mod alias */")
            }
            None => {
                error!("未解決の別名をCの式にしようとしました {:?}", location);
                CExpr::Other("/* error: unresolved alias */")
            }
        },
        KTerm::Const { k_const, .. } => gen_const_data((*k_const).of(&cx.outlines.consts)),
        KTerm::StaticVar { static_var, .. } => gen_static_var_term(*static_var, cx),
        KTerm::Fn { k_fn, .. } => gen_fn_term(*k_fn, cx),
        KTerm::Label { label, .. } => CExpr::Name(unique_label_name(*label, cx)),
        KTerm::Return { k_fn, .. } => {
            error!("can't gen return term to c {}", unique_fn_name(*k_fn, cx));
            CExpr::Other("/* error */ 0")
        }
        KTerm::ExternFn { extern_fn, .. } => gen_extern_fn_term(*extern_fn, cx),
        KTerm::Name(symbol) => CExpr::Name(unique_name(&symbol, cx)),
        KTerm::RecordTag { k_struct, .. } => gen_record_tag(*k_struct, &cx.outlines.structs),
        KTerm::FieldTag(KFieldTag { name, location }) => {
            error!("can't gen field term to c {} ({:?})", name, location);
            CExpr::Other("/* error */ 0")
        }
    }
}

fn gen_unary_op(
    op: CUnaryOp,
    args: &[KTerm],
    results: &[KSymbol],
    conts: &[KNode],
    ty_env: &KTyEnv,
    cx: &mut Cx,
) {
    match (args, results, conts) {
        ([arg], [result], [cont]) => {
            let arg = gen_term(arg, cx);
            let expr = arg.into_unary_op(op);
            emit_var_decl(result, Some(expr), ty_env, cx);
            gen_node(cont, ty_env, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_binary_op(
    op: CBinaryOp,
    args: &[KTerm],
    results: &[KSymbol],
    conts: &[KNode],
    ty_env: &KTyEnv,
    cx: &mut Cx,
) {
    match (args, results, conts) {
        ([left, right], [result], [cont]) => {
            let left = gen_term(left, cx);
            let right = gen_term(right, cx);
            let expr = left.into_binary_op(op, right);
            emit_var_decl(result, Some(expr), ty_env, cx);
            gen_node(cont, ty_env, cx);
        }
        _ => unimplemented!(),
    }
}

fn emit_assign(
    op: CBinaryOp,
    args: &[KTerm],
    _results: &[KSymbol],
    conts: &[KNode],
    ty_env: &KTyEnv,
    cx: &mut Cx,
) {
    match (args, conts) {
        ([left, right], [cont]) => {
            match left {
                KTerm::Name(symbol) if !symbol.local.of(&cx.locals).is_alive => {
                    cx.stmts.push(CStmt::Comment(format!(
                        "assignment to {} is eliminated.",
                        symbol.local.of(&cx.locals).name
                    )));
                }
                _ => {
                    let left = gen_term(left, cx);
                    let right = gen_term(right, cx);
                    cx.stmts.push(
                        left.into_unary_op(CUnaryOp::Deref)
                            .into_binary_op(op, right)
                            .into_stmt(),
                    );
                }
            }

            gen_node(cont, ty_env, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_node(node: &KNode, ty_env: &KTyEnv, cx: &mut Cx) {
    let tys = node.tys.as_slice();
    let args = node.args.as_slice();
    let results = node.results.as_slice();
    let conts = node.conts.as_slice();

    match node.prim {
        KPrim::Stuck => unreachable!(),
        KPrim::Jump => match (args, results) {
            ([KTerm::Return { .. }, KTerm::Unit { .. }], []) | ([KTerm::Return { .. }], []) => {
                cx.stmts.push(CStmt::Return(None));
            }
            ([KTerm::Return { .. }, arg], []) => {
                let arg = gen_term(arg, cx);
                cx.stmts.push(CStmt::Return(Some(arg)));
            }
            ([KTerm::Label { label, .. }, args @ ..], []) => {
                let name = unique_label_name(*label, cx);
                let params = cx.label_param_lists[*label].to_owned();

                for (param, arg) in params.into_iter().zip(args) {
                    let name = unique_name(&param, cx);

                    if !param.local.of(&cx.locals).is_alive {
                        CStmt::Comment(format!("{} is skipped.", &name));
                        continue;
                    }

                    let arg = gen_term(arg, cx);

                    cx.stmts.push(
                        CExpr::Name(name)
                            .into_binary_op(CBinaryOp::Assign, arg)
                            .into_stmt(),
                    );
                }

                cx.stmts.push(CStmt::Goto { label: name });
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect => match (results, conts) {
            ([result], [cont]) => {
                let call_expr = {
                    let mut args = args.iter();
                    let left = gen_term(args.next().unwrap(), cx);
                    let args = args.map(|arg| gen_term(arg, cx));
                    left.into_call(args)
                };
                emit_var_decl(result, Some(call_expr), ty_env, cx);
                gen_node(cont, ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Let => match (args, results, conts) {
            ([init], [result], [cont]) => {
                let init = gen_term(init, cx);
                emit_var_decl(result, Some(init), ty_env, cx);
                gen_node(cont, ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Record => match (tys, results, conts) {
            ([ty], [result], [cont]) => {
                let k_struct = ty.as_struct(ty_env).unwrap();

                let (name, ty) = gen_param(result, ty_env, cx);
                cx.stmts.push(CStmt::VarDecl {
                    storage_modifier_opt: None,
                    name: name.clone(),
                    ty,
                    init_opt: None,
                });

                let self_name = match k_struct.ty(&cx.outlines.structs) {
                    KTy::Struct(_) => CExpr::Name(name.clone()),
                    KTy::Enum(_) => {
                        // タグを設定する。
                        let left = CExpr::Name(name.clone()).into_dot("tag_");
                        let right = gen_record_tag(k_struct, &cx.outlines.structs);
                        cx.stmts
                            .push(left.into_binary_op(CBinaryOp::Assign, right).into_stmt());

                        CExpr::Name(name.clone()).into_dot(unique_struct_name(k_struct, cx))
                    }
                    _ => unreachable!(),
                };

                let outlines = cx.outlines;
                for (arg, field) in args.iter().zip(k_struct.fields(&outlines.structs)) {
                    let left = self_name.clone().into_dot(field.name(&outlines.fields));
                    let right = gen_term(arg, cx);
                    cx.stmts
                        .push(left.into_binary_op(CBinaryOp::Assign, right).into_stmt());
                }

                gen_node(cont, ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::GetField | KPrim::GetFieldMut => match (args, results, conts) {
            (
                [left, KTerm::FieldTag(KFieldTag {
                    name: field_name, ..
                })],
                [result],
                [cont],
            ) => {
                let left = gen_term(left, cx);
                let expr = left.into_arrow(field_name).into_ref();
                emit_var_decl(result, Some(expr), ty_env, cx);
                gen_node(cont, ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::If => match (args, results, conts) {
            ([cond], [], [then_cont, else_cont]) => {
                let cond = gen_term(cond, cx);
                let then_cont = gen_node_as_block(then_cont, ty_env, cx);
                let else_cont = gen_node_as_block(else_cont, ty_env, cx);

                let body = Box::new(CStmt::Block(then_cont));
                let alt = Box::new(CStmt::Block(else_cont));
                cx.stmts.push(CStmt::If { cond, body, alt });
            }
            _ => unimplemented!(),
        },
        KPrim::Switch => match args {
            [cond, pats @ ..] => {
                // FIXME: label_sigs
                let is_tagged_union = ty_env
                    .as_enum(&cond.ty(&cx.outlines, &KLabelSigArena::default(), &cx.locals))
                    .map_or(false, |k_enum| k_enum.is_tagged_union(&cx.outlines.enums));

                let mut cond = gen_term(cond, cx);
                if is_tagged_union {
                    cond = cond.into_dot("tag_");
                }

                let mut cases = vec![];
                let mut default_opt = None;

                assert_eq!(pats.len(), conts.len());
                for (pat, cont) in pats.iter().zip(conts) {
                    if let KTerm::Name(_) = pat {
                        // _ パターン
                        if default_opt.is_some() {
                            continue;
                        }

                        let body = gen_node_as_block(cont, ty_env, cx);
                        default_opt = Some(body);
                    } else {
                        // 定数パターン
                        let pat = gen_term(pat, cx);
                        let body = gen_node_as_block(cont, ty_env, cx);
                        cases.push((pat, body));
                    }
                }

                cx.stmts.push(CStmt::Switch {
                    cond,
                    cases,
                    default_opt,
                });
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => gen_unary_op(CUnaryOp::Deref, args, results, conts, ty_env, cx),
        KPrim::Ref | KPrim::RefMut => gen_unary_op(CUnaryOp::Ref, args, results, conts, ty_env, cx),
        KPrim::Minus => gen_unary_op(CUnaryOp::Minus, args, results, conts, ty_env, cx),
        KPrim::Not => match args {
            [arg] => {
                let op = if arg
                    .ty(&cx.outlines, &VecArena::default(), &cx.locals)
                    .is_bool(ty_env)
                {
                    CUnaryOp::LogNot
                } else {
                    CUnaryOp::BitNot
                };
                gen_unary_op(op, args, results, conts, ty_env, cx)
            }
            _ => unreachable!(),
        },
        KPrim::Add => gen_binary_op(CBinaryOp::Add, args, results, conts, ty_env, cx),
        KPrim::Sub => gen_binary_op(CBinaryOp::Sub, args, results, conts, ty_env, cx),
        KPrim::Mul => gen_binary_op(CBinaryOp::Mul, args, results, conts, ty_env, cx),
        KPrim::Div => gen_binary_op(CBinaryOp::Div, args, results, conts, ty_env, cx),
        KPrim::Modulo => gen_binary_op(CBinaryOp::Modulo, args, results, conts, ty_env, cx),
        KPrim::Equal => gen_binary_op(CBinaryOp::Equal, args, results, conts, ty_env, cx),
        KPrim::NotEqual => gen_binary_op(CBinaryOp::NotEqual, args, results, conts, ty_env, cx),
        KPrim::LessThan => gen_binary_op(CBinaryOp::LessThan, args, results, conts, ty_env, cx),
        KPrim::LessEqual => gen_binary_op(CBinaryOp::LessEqual, args, results, conts, ty_env, cx),
        KPrim::GreaterThan => {
            gen_binary_op(CBinaryOp::GreaterThan, args, results, conts, ty_env, cx)
        }
        KPrim::GreaterEqual => {
            gen_binary_op(CBinaryOp::GreaterEqual, args, results, conts, ty_env, cx)
        }
        KPrim::BitAnd => gen_binary_op(CBinaryOp::BitAnd, args, results, conts, ty_env, cx),
        KPrim::BitOr => gen_binary_op(CBinaryOp::BitOr, args, results, conts, ty_env, cx),
        KPrim::BitXor => gen_binary_op(CBinaryOp::BitXor, args, results, conts, ty_env, cx),
        KPrim::LeftShift => gen_binary_op(CBinaryOp::LeftShift, args, results, conts, ty_env, cx),
        KPrim::RightShift => gen_binary_op(CBinaryOp::RightShift, args, results, conts, ty_env, cx),
        KPrim::Cast => match (args, results, conts) {
            ([arg], [result], [cont]) => {
                let arg = gen_term(arg, cx);
                let result_ty = gen_ty(&result.ty(&cx.locals).to_ty1(), ty_env, cx);
                let expr = arg.into_cast(result_ty);
                emit_var_decl(result, Some(expr), ty_env, cx);
                gen_node(cont, ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Assign => emit_assign(CBinaryOp::Assign, args, results, conts, ty_env, cx),
        KPrim::AddAssign => emit_assign(CBinaryOp::AddAssign, args, results, conts, ty_env, cx),
        KPrim::SubAssign => emit_assign(CBinaryOp::SubAssign, args, results, conts, ty_env, cx),
        KPrim::MulAssign => emit_assign(CBinaryOp::MulAssign, args, results, conts, ty_env, cx),
        KPrim::DivAssign => emit_assign(CBinaryOp::DivAssign, args, results, conts, ty_env, cx),
        KPrim::ModuloAssign => {
            emit_assign(CBinaryOp::ModuloAssign, args, results, conts, ty_env, cx)
        }
        KPrim::BitAndAssign => {
            emit_assign(CBinaryOp::BitAndAssign, args, results, conts, ty_env, cx)
        }
        KPrim::BitOrAssign => emit_assign(CBinaryOp::BitOrAssign, args, results, conts, ty_env, cx),
        KPrim::BitXorAssign => {
            emit_assign(CBinaryOp::BitXorAssign, args, results, conts, ty_env, cx)
        }
        KPrim::LeftShiftAssign => {
            emit_assign(CBinaryOp::LeftShiftAssign, args, results, conts, ty_env, cx)
        }
        KPrim::RightShiftAssign => emit_assign(
            CBinaryOp::RightShiftAssign,
            args,
            results,
            conts,
            ty_env,
            cx,
        ),
    }
}

fn gen_node_as_block(node: &KNode, ty_env: &KTyEnv, cx: &mut Cx) -> CBlock {
    let stmts = cx.enter_block(|cx| gen_node(node, ty_env, cx));
    CBlock { stmts }
}

fn gen_fn_sig(
    params: &[KSymbol],
    result_ty: &KTy,
    ty_env: &KTyEnv,
    cx: &mut Cx,
) -> (Vec<(String, CTy)>, CTy) {
    let params = params
        .into_iter()
        .map(|param| gen_param(param, ty_env, cx))
        .collect();
    let result_ty = gen_ty(result_ty, ty_env, cx);
    (params, result_ty)
}

/// 宣言を生成する。
fn gen_root_for_decls(root: &KModData, cx: &mut Cx) {
    let outlines = cx.outlines;
    let empty_ty_env = KTyEnv::default();

    cx.static_var_ident_ids
        .resize(outlines.static_vars.len(), None);
    cx.fn_ident_ids.resize(outlines.fns.len(), None);
    cx.enum_ident_ids.resize(outlines.enums.len(), None);
    cx.struct_ident_ids.resize(outlines.structs.len(), None);

    for (k_struct, struct_data) in outlines.structs.enumerate() {
        let name = unique_struct_name(k_struct, cx);
        let fields = struct_data
            .fields
            .iter()
            .map(|field| {
                (
                    field.name(&cx.outlines.fields).to_string(),
                    gen_ty(field.ty(&cx.outlines.fields), &empty_ty_env, cx),
                )
            })
            .collect();
        cx.decls.push(CStmt::StructDecl {
            name,
            fields,
            union_opt: None,
        });
    }

    for (k_enum, enum_data) in outlines.enums.enumerate() {
        match &enum_data.repr {
            KEnumRepr::Never | KEnumRepr::Unit | KEnumRepr::Const { .. } => continue,
            KEnumRepr::Sum { tag_ty } => {
                let name = unique_enum_name(k_enum, cx);
                let fields = {
                    let tag_ty = gen_ty(tag_ty, &empty_ty_env, cx);
                    vec![("tag_".to_string(), tag_ty)]
                };
                let variants = enum_data
                    .variants
                    .iter()
                    .filter_map(|variant| match variant {
                        KVariant::Const(_) => None,
                        KVariant::Record(k_struct) => {
                            let name = unique_struct_name(*k_struct, cx);
                            let ty = CTy::Struct(name.to_string());
                            Some((name, ty))
                        }
                    })
                    .collect();
                cx.decls.push(CStmt::StructDecl {
                    name,
                    fields,
                    union_opt: Some(variants),
                });
            }
        }
    }

    for (static_var, static_var_data) in outlines.static_vars.enumerate() {
        let name = unique_static_var_name(static_var, cx);
        let ty = gen_ty(&static_var_data.ty, &empty_ty_env, cx);
        let init_opt = static_var_data.value_opt.as_ref().map(gen_constant_value);
        cx.decls.push(CStmt::VarDecl {
            storage_modifier_opt: Some(CStorageModifier::Static),
            name,
            ty,
            init_opt,
        });
    }

    for (extern_fn, extern_fn_data) in root.extern_fns.enumerate() {
        let params = extern_fn_data.params.as_slice();
        let locals = &extern_fn_data.locals;

        // FIXME: clone しない
        cx.locals = locals.clone();
        cx.local_ident_ids.clear();
        cx.local_ident_ids.resize(cx.locals.len(), None);

        let name = unique_extern_fn_name(extern_fn, cx);
        let (params, result_ty) = {
            let result_ty = extern_fn.result_ty(&outlines.extern_fns);
            gen_fn_sig(params, result_ty, &empty_ty_env, cx)
        };
        cx.decls.push(CStmt::ExternFnDecl {
            name,
            params,
            result_ty,
        });

        cx.locals = VecArena::default();
    }

    // 関数宣言
    for (k_fn, fn_data) in root.fns.enumerate() {
        let name = unique_fn_name(k_fn, cx);
        let params = fn_data
            .params
            .iter()
            .map(|symbol| {
                let name = symbol.local.name(&fn_data.locals).to_string();
                let ty = gen_ty(
                    &symbol.local.ty(&fn_data.locals).to_ty1(),
                    &empty_ty_env,
                    cx,
                );
                (name, ty)
            })
            .collect();
        let result_ty = gen_ty(k_fn.result_ty(&outlines.fns), &empty_ty_env, cx);

        cx.decls.push(CStmt::FnDecl {
            name,
            params,
            result_ty,
            body_opt: None,
        });
    }
}

/// 定義を生成する。
fn gen_root_for_defs(root: &KModData, cx: &mut Cx) {
    for (k_fn, fn_data) in root.fns.enumerate() {
        let params = fn_data.params.as_slice();
        let locals = &fn_data.locals;
        let labels = &fn_data.labels;
        let ty_env = &fn_data.ty_env;

        // FIXME: clone しない
        cx.locals = locals.clone();
        cx.local_ident_ids.clear();
        cx.local_ident_ids.resize(cx.locals.len(), None);

        let stmts = cx.enter_block(|cx| {
            for label_data in labels.iter() {
                for param in label_data.params.iter() {
                    emit_var_decl(param, None, &ty_env, cx);
                }
            }

            cx.label_raw_names =
                VecArena::from_iter(labels.iter().map(|label| label.name.to_string()));
            cx.label_param_lists =
                VecArena::from_iter(labels.iter().map(|label| label.params.to_owned()));
            cx.label_ident_ids.clear();
            cx.label_ident_ids.resize(labels.len(), None);
            gen_node(&fn_data.body, &ty_env, cx);

            for (label, label_data) in labels.enumerate() {
                let name = unique_label_name(label, cx);
                cx.stmts.push(CStmt::Label { label: name });

                let body = &label_data.body;
                gen_node(body, &ty_env, cx);
            }
        });

        let name = unique_fn_name(k_fn, cx);
        let (params, result_ty) = {
            let result_ty = k_fn.result_ty(&cx.outlines.fns);
            gen_fn_sig(params, result_ty, KTyEnv::EMPTY, cx)
        };
        cx.decls.push(CStmt::FnDecl {
            name,
            params,
            result_ty,
            body_opt: Some(CBlock { stmts }),
        });

        cx.locals = VecArena::default();
    }
}

pub(crate) fn gen(mod_outlines: &KModOutlines, mods: &KModArena) -> CRoot {
    let mut decls = vec![];

    // 宣言
    let cxx = mod_outlines
        .iter()
        .zip(mods.iter())
        .map(|(mod_outline, mod_data)| {
            let mut cx = Cx::new(mod_outline, mod_outlines);
            gen_root_for_decls(mod_data, &mut cx);
            decls.extend(cx.decls.drain(..));
            (mod_outline, mod_data, cx)
        })
        .collect::<Vec<_>>();

    // 定義
    for (_, mod_data, mut cx) in cxx {
        gen_root_for_defs(mod_data, &mut cx);
        decls.extend(cx.decls.drain(..));
    }

    CRoot { decls }
}
