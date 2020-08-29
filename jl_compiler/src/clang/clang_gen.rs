//! CPS 中間表現をC言語のコードに変換する処理

use super::*;
use crate::{token::eval_number, utils::VecArena};
use c_stmt::CStorageModifier;
use std::{
    collections::HashMap,
    mem::{replace, take},
};

type IdentMap = HashMap<String, IdProvider>;

/// C code generation context.
struct Cx<'a> {
    k_mod: KMod,
    mod_outline: &'a KModOutline,
    mod_outlines: &'a KModOutlines,
    ident_map: HashMap<String, IdProvider>,
    static_var_ident_ids: Vec<Option<usize>>,
    fn_ident_ids: Vec<Option<usize>>,
    enum_ident_ids: Vec<Option<usize>>,
    struct_ident_ids: Vec<Option<usize>>,
    local_vars: KLocalVarArena,
    local_var_ident_ids: Vec<Option<usize>>,
    label_raw_names: VecArena<KLabelTag, String>,
    label_param_lists: VecArena<KLabelTag, Vec<KSymbol>>,
    label_ident_ids: Vec<Option<usize>>,
    stmts: Vec<CStmt>,
    decls: Vec<CStmt>,
}

impl<'a> Cx<'a> {
    fn new(k_mod: KMod, mod_outline: &'a KModOutline, mod_outlines: &'a KModOutlines) -> Self {
        Self {
            k_mod,
            mod_outline,
            mod_outlines,
            ident_map: Default::default(),
            static_var_ident_ids: Default::default(),
            fn_ident_ids: Default::default(),
            enum_ident_ids: Default::default(),
            struct_ident_ids: Default::default(),
            local_vars: Default::default(),
            local_var_ident_ids: Default::default(),
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
    unique_local_var_name(symbol.local_var, cx)
}

fn unique_static_var_name(static_var: KStaticVar, cx: &mut Cx) -> String {
    do_unique_name(
        static_var.to_index(),
        static_var.name(&cx.mod_outline.static_vars),
        &mut cx.static_var_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_fn_name(k_fn: KFn, cx: &mut Cx) -> String {
    // pub な関数の名前はマングルしない。
    if k_fn.is_pub(&cx.mod_outline.fns) {
        return k_fn.name(&cx.mod_outline.fns).to_string();
    }

    do_unique_name(
        k_fn.to_index(),
        k_fn.name(&cx.mod_outline.fns),
        &mut cx.fn_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_extern_fn_name(extern_fn: KExternFn, cx: &mut Cx) -> String {
    // 外部関数の名前は勝手にマングルしない。
    extern_fn.name(&cx.mod_outline.extern_fns).to_string()
}

fn unique_struct_enum_name(struct_enum: KStructEnum, cx: &mut Cx) -> String {
    struct_enum.name(&cx.mod_outline.struct_enums).to_string()
}

fn unique_struct_name(k_mod: KMod, k_struct: KStruct, cx: &mut Cx) -> String {
    k_struct
        .name(&k_mod.of(&cx.mod_outlines).structs)
        .to_string()
    // do_unique_name(
    //     k_struct.to_index(),
    //     &cx.mod_outline.structs[k_struct].name,
    //     &mut cx.struct_ident_ids,
    //     &mut cx.ident_map,
    // )
}

fn unique_local_var_name(local_var: KLocalVar, cx: &mut Cx) -> String {
    do_unique_name(
        local_var.to_index(),
        &cx.local_vars[local_var].name,
        &mut cx.local_var_ident_ids,
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
    let is_alive = symbol.local_var.of(&cx.local_vars).is_alive;
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

fn gen_basic_ty(basic_ty: KNumberTy) -> CTy {
    match basic_ty {
        KNumberTy::Bool | KNumberTy::I32 | KNumberTy::CNN => CTy::Int,
        KNumberTy::I8 => CTy::SignedChar,
        KNumberTy::I16 => CTy::Short,
        KNumberTy::I64 | KNumberTy::Isize | KNumberTy::INN => CTy::LongLong,
        KNumberTy::U8 | KNumberTy::C8 => CTy::UnsignedChar,
        KNumberTy::U16 | KNumberTy::C16 => CTy::UnsignedShort,
        KNumberTy::U32 | KNumberTy::C32 => CTy::UnsignedInt,
        KNumberTy::U64 | KNumberTy::Usize | KNumberTy::UNN => CTy::UnsignedLongLong,
        KNumberTy::F32 => CTy::Float,
        KNumberTy::F64 | KNumberTy::FNN => CTy::Double,
    }
}

fn gen_struct_enum_ty(k_mod: KMod, struct_enum: KStructEnum, cx: &mut Cx) -> CTy {
    // FIXME: unique_enum_name を事前に計算しておく
    let name = struct_enum
        .name(&k_mod.of(&cx.mod_outlines).struct_enums)
        .to_string();
    CTy::Struct(name)
}

fn gen_constant_value(value: &KConstValue) -> CExpr {
    match value {
        KConstValue::Bool(true) => CExpr::BoolLit("1"),
        KConstValue::Bool(false) => CExpr::BoolLit("0"),
        KConstValue::I8(value) => CExpr::IntLit(value.to_string()),
        KConstValue::I16(value) => CExpr::IntLit(value.to_string()),
        KConstValue::I32(value) => CExpr::IntLit(value.to_string()),
        KConstValue::I64(value) => CExpr::LongLongLit(value.to_string()),
        KConstValue::Isize(value) => CExpr::LongLongLit(value.to_string()),
        KConstValue::U8(value) => CExpr::IntLit(value.to_string()),
        KConstValue::U16(value) => CExpr::IntLit(value.to_string()),
        KConstValue::U32(value) => CExpr::IntLit(value.to_string()),
        KConstValue::U64(value) => CExpr::UnsignedLongLongLit(value.to_string()),
        KConstValue::Usize(value) => CExpr::UnsignedLongLongLit(value.to_string()),
        KConstValue::F32(value) => CExpr::FloatLit(value.to_string()),
        KConstValue::F64(value) => CExpr::DoubleLit(value.to_string()),
        KConstValue::C8(value) => CExpr::IntLit(value.to_string()),
        KConstValue::C16(value) => CExpr::IntLit(value.to_string()),
        KConstValue::C32(value) => CExpr::IntLit(value.to_string()),
    }
}

fn gen_invalid_constant_value() -> CExpr {
    CExpr::Other("/* invalid const */ 0")
}

fn gen_record_tag(k_struct: KStruct, structs: &KStructArena) -> CExpr {
    // OK: タグ値は決定済みのはず
    let tag = k_struct.tag_value_opt(structs).unwrap();
    CExpr::IntLit(tag.to_string())
}

fn gen_ty(ty: &KTy, ty_env: &KTyEnv, cx: &mut Cx) -> CTy {
    gen_ty2(&ty.erasure(cx.k_mod, cx.mod_outlines), ty_env, cx)
}

fn gen_ty2(ty: &KTy2, ty_env: &KTyEnv, cx: &mut Cx) -> CTy {
    match ty {
        KTy2::Unresolved { cause } => {
            error!("Unexpected unresolved type {:?} (cause={:?})", ty, cause);
            CTy::Other("/* unresolved */ void")
        }
        KTy2::Meta(meta_ty) => match meta_ty.try_unwrap(&ty_env) {
            Some(ty) => gen_ty2(&ty.borrow(), ty_env, cx),
            None => {
                error!("Unexpected free type {:?}", meta_ty);
                CTy::Other("/* free */ void")
            }
        },
        KTy2::Var(_) => CTy::Other("/* ty var */ void"),
        KTy2::Unknown => CTy::Other("/* unknown */ void"),
        KTy2::Never => CTy::Other("/* never */ void"),
        KTy2::Unit => CTy::Void,
        KTy2::Number(basic_ty) => gen_basic_ty(*basic_ty),
        KTy2::Fn {
            param_tys,
            result_ty,
        } => {
            let param_tys = param_tys.iter().map(|ty| gen_ty2(ty, ty_env, cx)).collect();
            let result_ty = Box::new(gen_ty2(&result_ty, ty_env, cx));
            CTy::FnPtr {
                param_tys,
                result_ty,
            }
        }
        KTy2::Ptr { k_mut, base_ty } => {
            let base_ty = gen_ty2(&base_ty, ty_env, cx);
            match k_mut {
                KMut::Const => base_ty.into_const().into_ptr(),
                KMut::Mut => base_ty.into_ptr(),
            }
        }
        KTy2::ConstEnum(k_mod, const_enum) => gen_ty(
            const_enum.repr_ty(&k_mod.of(cx.mod_outlines).const_enums),
            ty_env,
            cx,
        ),
        &KTy2::StructEnum(k_mod, struct_enum) => gen_struct_enum_ty(k_mod, struct_enum, cx),
        KTy2::Struct(k_mod, k_struct) => {
            // FIXME: unique_struct_name を事前に計算しておく
            let name = k_struct.name(&k_mod.of(&cx.mod_outlines).structs);
            CTy::Struct(name.to_string())
        }
        KTy2::App { k_struct, .. } => {
            let name = &k_struct.of(cx.mod_outlines).name;
            CTy::Struct(name.to_string())
        }
    }
}

fn gen_param(param: &KSymbol, ty_env: &KTyEnv, cx: &mut Cx) -> (String, CTy) {
    let name = unique_name(param, cx);
    let ty = param.ty(&cx.local_vars);
    (name, gen_ty2(&ty, &ty_env, cx))
}

fn gen_const(const_outline: &KConstOutline) -> CExpr {
    match &const_outline.value_opt {
        Some(value) => gen_constant_value(value),
        None => gen_invalid_constant_value(),
    }
}

fn gen_static_var_term(static_var: KProjectStaticVar, cx: &mut Cx) -> CExpr {
    let KProjectStaticVar(k_mod, static_var) = static_var;
    if k_mod != cx.k_mod {
        // 宣言側のモジュールでどのような名前にマングリングされたか分からないので参照できない
        unimplemented!("use された static 変数の使用は未実装です")
    }
    CExpr::Name(unique_static_var_name(static_var, cx))
}

fn gen_fn_term(k_fn: KFn, cx: &mut Cx) -> CExpr {
    CExpr::Name(unique_fn_name(k_fn, cx))
}

fn gen_extern_fn_term(extern_fn: KExternFn, cx: &mut Cx) -> CExpr {
    CExpr::Name(unique_extern_fn_name(extern_fn, cx))
}

fn gen_number(s: &str, ty: &KTy2) -> CExpr {
    let (value, _) = match eval_number(s) {
        Ok(pair) => pair,
        Err(_) => return CExpr::Other("/* error number */ (void)0"),
    };

    match (value, ty) {
        (KNumber::INN(value), &KTy2::I8)
        | (KNumber::INN(value), &KTy2::I16)
        | (KNumber::INN(value), &KTy2::I32) => CExpr::IntLit(value.to_string()),
        (KNumber::UNN(value), &KTy2::I8)
        | (KNumber::UNN(value), &KTy2::I16)
        | (KNumber::UNN(value), &KTy2::I32)
        | (KNumber::UNN(value), &KTy2::U8)
        | (KNumber::UNN(value), &KTy2::U16)
        | (KNumber::UNN(value), &KTy2::U32)
        | (KNumber::UNN(value), &KTy2::C8)
        | (KNumber::UNN(value), &KTy2::C16)
        | (KNumber::UNN(value), &KTy2::C32) => CExpr::IntLit(value.to_string()),
        (KNumber::UNN(value), &KTy2::I64) | (KNumber::UNN(value), &KTy2::ISIZE) => {
            CExpr::LongLongLit(value.to_string())
        }
        (KNumber::FNN(value), &KTy2::C8)
        | (KNumber::FNN(value), &KTy2::C16)
        | (KNumber::FNN(value), &KTy2::C32)
        | (KNumber::FNN(value), &KTy2::CNN) => CExpr::IntLit(value.to_string()),
        (KNumber::INN(value), &KTy2::I64)
        | (KNumber::INN(value), &KTy2::ISIZE)
        | (KNumber::INN(value), &KTy2::INN) => CExpr::LongLongLit(value.to_string()),
        (KNumber::UNN(value), &KTy2::U64)
        | (KNumber::UNN(value), &KTy2::USIZE)
        | (KNumber::UNN(value), &KTy2::UNN) => CExpr::UnsignedLongLongLit(value.to_string()),
        (KNumber::FNN(value), &KTy2::F32)
        | (KNumber::FNN(value), &KTy2::F64)
        | (KNumber::FNN(value), &KTy2::FNN) => CExpr::DoubleLit(value.to_string()),
        _ => {
            log::error!("[BUG] invalid number literal {:?}", s);
            CExpr::Other("/* invalid number */ (void)0")
        }
    }
}

fn gen_term(term: &KTerm, cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Unit { .. } => {
            // FIXME: error!
            CExpr::Other("(void)0")
        }
        KTerm::Int { text, ty, .. } => gen_number(text, ty),
        KTerm::Float { text, ty, .. } => gen_number(text, ty),
        KTerm::Char { text, .. } => CExpr::CharLit(text.to_string()),
        KTerm::Str { text, .. } => CExpr::Cast {
            ty: CTy::UnsignedChar.into_const().into_ptr(),
            arg: Box::new(CExpr::StrLit(text.to_string())),
        },
        KTerm::True { .. } => CExpr::BoolLit("1"),
        KTerm::False { .. } => CExpr::BoolLit("0"),
        KTerm::Alias { alias, loc } => match alias.of(&cx.mod_outline.aliases).referent() {
            Some(KProjectSymbol::Const(k_const)) => gen_const(k_const.of(cx.mod_outlines)),
            Some(KProjectSymbol::StaticVar(static_var)) => gen_static_var_term(static_var, cx),
            Some(KProjectSymbol::Fn(k_fn)) => {
                let fn_name = k_fn.of(cx.mod_outlines).name.to_string();
                CExpr::Name(fn_name)
            }
            Some(KProjectSymbol::ExternFn(extern_fn)) => {
                let extern_fn_name = extern_fn.of(cx.mod_outlines).name.to_string();
                CExpr::Name(extern_fn_name)
            }
            Some(KProjectSymbol::ConstEnum(..))
            | Some(KProjectSymbol::StructEnum(..))
            | Some(KProjectSymbol::Struct(..)) => {
                error!("別名の参照先が不正です {:?}", (term, loc));
                CExpr::Other("/* error: invalid alias term ")
            }
            Some(KProjectSymbol::Mod(_)) => {
                error!("モジュールを指す別名はCの式になりません {:?}", loc);
                CExpr::Other("/* error: mod alias */")
            }
            None => {
                error!("未解決の別名をCの式にしようとしました {:?}", loc);
                CExpr::Other("/* error: unresolved alias */")
            }
        },
        KTerm::Const { k_mod, k_const, .. } => {
            gen_const((*k_const).of(&k_mod.of(cx.mod_outlines).consts))
        }
        KTerm::StaticVar { static_var, .. } => {
            gen_static_var_term(KProjectStaticVar(cx.k_mod, *static_var), cx)
        }
        KTerm::Fn { k_fn, .. } => gen_fn_term(*k_fn, cx),
        KTerm::Label { label, .. } => CExpr::Name(unique_label_name(*label, cx)),
        KTerm::Return { k_fn, .. } => {
            error!("can't gen return term to c {}", unique_fn_name(*k_fn, cx));
            CExpr::Other("/* error */ 0")
        }
        KTerm::ExternFn { extern_fn, .. } => gen_extern_fn_term(*extern_fn, cx),
        KTerm::Name(symbol) => CExpr::Name(unique_name(&symbol, cx)),
        KTerm::RecordTag {
            k_mod, k_struct, ..
        } => gen_record_tag(*k_struct, &k_mod.of(&cx.mod_outlines).structs),
        KTerm::FieldTag(KFieldTag { name, loc }) => {
            error!("can't gen field term to c {} ({:?})", name, loc);
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
                KTerm::Name(symbol) if !symbol.local_var.of(&cx.local_vars).is_alive => {
                    cx.stmts.push(CStmt::Comment(format!(
                        "assignment to {} is eliminated.",
                        symbol.local_var.of(&cx.local_vars).name
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

                    if !param.local_var.of(&cx.local_vars).is_alive {
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
                let (k_mod, k_struct) = ty.as_struct(ty_env).unwrap();

                let (name, ty) = gen_param(result, ty_env, cx);
                cx.stmts.push(CStmt::VarDecl {
                    storage_modifier_opt: None,
                    name: name.clone(),
                    ty,
                    init_opt: None,
                });

                let self_name = match k_struct.ty(&k_mod.of(cx.mod_outlines).structs) {
                    KTy::Struct(_) | KTy::StructGeneric { .. } => CExpr::Name(name.clone()),
                    KTy::StructEnum(_) => {
                        // タグを設定する。
                        let left = CExpr::Name(name.clone()).into_dot("tag_");
                        let right = gen_record_tag(k_struct, &k_mod.of(cx.mod_outlines).structs);
                        cx.stmts
                            .push(left.into_binary_op(CBinaryOp::Assign, right).into_stmt());

                        CExpr::Name(name.clone()).into_dot(unique_struct_name(k_mod, k_struct, cx))
                    }
                    _ => unreachable!(),
                };

                for (arg, field) in args
                    .iter()
                    .zip(k_struct.fields(&k_mod.of(cx.mod_outlines).structs))
                {
                    let left = self_name
                        .clone()
                        .into_dot(field.name(&k_mod.of(cx.mod_outlines).fields));
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
                let is_enum = cond
                    .ty(
                        cx.k_mod,
                        &cx.mod_outline,
                        &KLabelSigArena::default(),
                        &cx.local_vars,
                        cx.mod_outlines,
                    )
                    .as_enum(ty_env)
                    .is_some();

                let mut cond = gen_term(cond, cx);
                if is_enum {
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
                    .ty(
                        cx.k_mod,
                        &cx.mod_outline,
                        &VecArena::default(),
                        &cx.local_vars,
                        cx.mod_outlines,
                    )
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
                let result_ty = gen_ty2(&result.ty(&cx.local_vars), ty_env, cx);
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
    let empty_ty_env = KTyEnv::default();

    cx.static_var_ident_ids
        .resize(cx.mod_outline.static_vars.len(), None);
    cx.fn_ident_ids.resize(cx.mod_outline.fns.len(), None);
    cx.enum_ident_ids
        .resize(cx.mod_outline.struct_enums.len(), None);
    cx.struct_ident_ids
        .resize(cx.mod_outline.structs.len(), None);

    for (k_struct, struct_data) in cx.mod_outline.structs.enumerate() {
        let name = unique_struct_name(cx.k_mod, k_struct, cx);
        let fields = struct_data
            .fields
            .iter()
            .map(|field| {
                (
                    field.name(&cx.mod_outline.fields).to_string(),
                    gen_ty(field.ty(&cx.mod_outline.fields), &empty_ty_env, cx),
                )
            })
            .collect();
        cx.decls.push(CStmt::StructDecl {
            name,
            fields,
            union_opt: None,
        });
    }

    for (struct_enum, enum_data) in cx.mod_outline.struct_enums.enumerate() {
        let tag_ty = enum_data.tag_ty();
        let name = unique_struct_enum_name(struct_enum, cx);
        let fields = {
            let tag_ty = gen_ty(&tag_ty, &empty_ty_env, cx);
            vec![("tag_".to_string(), tag_ty)]
        };
        let variants = enum_data
            .variants
            .iter()
            .filter_map(|&k_struct| {
                let name = unique_struct_name(cx.k_mod, k_struct, cx);
                let ty = CTy::Struct(name.to_string());
                Some((name, ty))
            })
            .collect();
        cx.decls.push(CStmt::StructDecl {
            name,
            fields,
            union_opt: Some(variants),
        });
    }

    for (static_var, static_var_outline) in cx.mod_outline.static_vars.enumerate() {
        let name = unique_static_var_name(static_var, cx);
        let ty = gen_ty(&static_var_outline.ty, &empty_ty_env, cx);
        let init_opt = static_var_outline
            .value_opt
            .as_ref()
            .map(gen_constant_value);
        cx.decls.push(CStmt::VarDecl {
            storage_modifier_opt: Some(CStorageModifier::Static),
            name,
            ty,
            init_opt,
        });
    }

    for (extern_fn, extern_fn_data) in root.extern_fns.enumerate() {
        let params = extern_fn_data.params.as_slice();
        let local_vars = &extern_fn_data.local_vars;

        // FIXME: clone しない
        cx.local_vars = local_vars.clone();
        cx.local_var_ident_ids.clear();
        cx.local_var_ident_ids.resize(cx.local_vars.len(), None);

        let name = unique_extern_fn_name(extern_fn, cx);
        let (params, result_ty) = {
            let result_ty = extern_fn.result_ty(&cx.mod_outline.extern_fns);
            gen_fn_sig(params, result_ty, &empty_ty_env, cx)
        };
        cx.decls.push(CStmt::ExternFnDecl {
            name,
            params,
            result_ty,
        });

        cx.local_vars = VecArena::default();
    }

    // 関数宣言
    for (k_fn, fn_data) in root.fns.enumerate() {
        let name = unique_fn_name(k_fn, cx);
        let params = fn_data
            .params
            .iter()
            .map(|symbol| {
                let name = symbol.local_var.name(&fn_data.local_vars).to_string();
                let ty = gen_ty2(&symbol.local_var.ty(&fn_data.local_vars), &empty_ty_env, cx);
                (name, ty)
            })
            .collect();
        let result_ty = gen_ty(k_fn.result_ty(&cx.mod_outline.fns), &empty_ty_env, cx);

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
        let local_vars = &fn_data.local_vars;
        let labels = &fn_data.labels;
        let ty_env = &fn_data.ty_env;

        // FIXME: clone しない
        cx.local_vars = local_vars.clone();
        cx.local_var_ident_ids.clear();
        cx.local_var_ident_ids.resize(cx.local_vars.len(), None);

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

            for (label, label_data) in labels.enumerate() {
                // 最初のラベルは関数のエントリーポイントなのでラベルとして出力する必要はない。
                if label.to_index() != 0 {
                    let name = unique_label_name(label, cx);
                    cx.stmts.push(CStmt::Label { label: name });
                }

                let body = &label_data.body;
                gen_node(body, &ty_env, cx);
            }
        });

        let name = unique_fn_name(k_fn, cx);
        let (params, result_ty) = {
            let result_ty = k_fn.result_ty(&cx.mod_outline.fns);
            gen_fn_sig(params, result_ty, KTyEnv::EMPTY, cx)
        };
        cx.decls.push(CStmt::FnDecl {
            name,
            params,
            result_ty,
            body_opt: Some(CBlock { stmts }),
        });

        cx.local_vars = VecArena::default();
    }
}

pub(crate) fn gen(mod_outlines: &KModOutlines, mods: &KModArena) -> CRoot {
    let mut decls = vec![];

    // 宣言
    let cxx = mod_outlines
        .enumerate()
        .zip(mods.iter())
        .map(|((k_mod, mod_outline), mod_data)| {
            let mut cx = Cx::new(k_mod, mod_outline, mod_outlines);
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
