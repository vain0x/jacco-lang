//! CPS 中間表現をC言語のコードに変換する処理

use super::*;
use std::collections::HashMap;
use std::{
    mem::{replace, take},
    rc::Rc,
};

type IdentMap = HashMap<String, IdProvider>;

/// C code generation context.
#[derive(Default)]
struct Cx {
    outlines: Rc<KOutlines>,
    ident_map: HashMap<String, IdProvider>,
    fn_ident_ids: Vec<Option<usize>>,
    struct_ident_ids: Vec<Option<usize>>,
    locals: Vec<KLocalData>,
    local_ident_ids: Vec<Option<usize>>,
    labels: Vec<KLabelData>,
    label_ident_ids: Vec<Option<usize>>,
    stmts: Vec<CStmt>,
    decls: Vec<CStmt>,
}

impl Cx {
    fn new(outlines: Rc<KOutlines>) -> Self {
        Self {
            outlines,
            ..Default::default()
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

fn unique_fn_name(k_fn: KFn, cx: &mut Cx) -> String {
    // pub な関数の名前はマングルしない。
    if cx.outlines.fn_get(k_fn).is_pub() {
        return k_fn.name(&cx.outlines).to_string();
    }

    do_unique_name(
        k_fn.id(),
        k_fn.name(&cx.outlines),
        &mut cx.fn_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_extern_fn_name(extern_fn: KExternFn, cx: &mut Cx) -> String {
    // 外部関数の名前は勝手にマングルしない。
    extern_fn.name(&cx.outlines).to_string()
}

fn unique_struct_name(k_struct: KStruct, cx: &mut Cx) -> String {
    do_unique_name(
        k_struct.id(),
        &cx.outlines.structs[k_struct.id()].name,
        &mut cx.struct_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_local_name(local: KLocal, cx: &mut Cx) -> String {
    do_unique_name(
        local.id(),
        &cx.locals[local.id()].name,
        &mut cx.local_ident_ids,
        &mut cx.ident_map,
    )
}

fn unique_label_name(label: KLabel, cx: &mut Cx) -> String {
    do_unique_name(
        label.id(),
        &cx.labels[label.id()].name,
        &mut cx.label_ident_ids,
        &mut cx.ident_map,
    )
}

fn emit_var_decl(symbol: KSymbol, init_opt: Option<CExpr>, ty_env: &KTyEnv, cx: &mut Cx) {
    let is_alive = cx.locals[symbol.local.id()].is_alive;
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

    cx.stmts.push(CStmt::VarDecl { name, ty, init_opt });
}

fn gen_ty(ty: KTy, ty_env: &KTyEnv, cx: &mut Cx) -> CTy {
    match ty {
        KTy::Unresolved => {
            error!("Unexpected unresolved type {:?}", ty);
            CTy::Other("/* unresolved */ void")
        }
        KTy::Meta(meta) => match meta.try_unwrap(&ty_env) {
            Some(ty) => {
                let ty = ty.borrow().clone();
                gen_ty(ty, ty_env, cx)
            }
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
        KTy::I32 | KTy::Bool => CTy::Int,
        KTy::I64 => CTy::LongLong,
        KTy::C8 => CTy::UnsignedChar,
        KTy::Ptr { ty } => gen_ty(*ty, ty_env, cx).into_ptr(),
        KTy::Struct(k_struct) => CTy::Struct(unique_struct_name(k_struct, cx)),
    }
}

fn gen_param(param: KSymbol, ty_env: &KTyEnv, cx: &mut Cx) -> (String, CTy) {
    let name = unique_name(&param, cx);
    let ty = param.ty(&cx.locals);
    (name, gen_ty(ty, &ty_env, cx))
}

fn gen_term(term: KTerm, cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Unit { .. } => {
            // FIXME: error!
            CExpr::IntLit("(void)0".to_string())
        }
        KTerm::Int(token) => CExpr::IntLit(token.into_text().replace("_", "")),
        KTerm::Char(token) => CExpr::CharLit(token.into_text()),
        KTerm::True(_) => CExpr::IntLit("1".to_string()),
        KTerm::False(_) => CExpr::IntLit("0".to_string()),
        KTerm::Fn(k_fn) => CExpr::Name(unique_fn_name(k_fn, cx)),
        KTerm::Label(label) => CExpr::Name(unique_label_name(label, cx)),
        KTerm::Return(k_fn) => {
            error!("can't gen return term to c {}", unique_fn_name(k_fn, cx));
            CExpr::IntLit("/* error */ 0".to_string())
        }
        KTerm::ExternFn(extern_fn) => CExpr::Name(unique_extern_fn_name(extern_fn, cx)),
        KTerm::Name(symbol) => CExpr::Name(unique_name(&symbol, cx)),
        KTerm::FieldTag(KFieldTag { name, location }) => {
            error!("can't gen field term to c {} ({:?})", name, location);
            CExpr::IntLit("/* error */ 0".to_string())
        }
    }
}

fn gen_unary_op(
    op: CUnaryOp,
    args: &mut Vec<KTerm>,
    results: &mut Vec<KSymbol>,
    conts: &mut Vec<KNode>,
    ty_env: &KTyEnv,
    cx: &mut Cx,
) {
    match (
        args.as_mut_slice(),
        results.as_mut_slice(),
        conts.as_mut_slice(),
    ) {
        ([arg], [result], [cont]) => {
            let arg = gen_term(take(arg), cx);
            let expr = arg.into_unary_op(op);
            emit_var_decl(take(result), Some(expr), ty_env, cx);
            gen_node(take(cont), ty_env, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_binary_op(
    op: CBinaryOp,
    args: &mut Vec<KTerm>,
    results: &mut Vec<KSymbol>,
    conts: &mut Vec<KNode>,
    ty_env: &KTyEnv,
    cx: &mut Cx,
) {
    match (
        args.as_mut_slice(),
        results.as_mut_slice(),
        conts.as_mut_slice(),
    ) {
        ([left, right], [result], [cont]) => {
            let left = gen_term(take(left), cx);
            let right = gen_term(take(right), cx);
            let expr = left.into_binary_op(op, right);
            emit_var_decl(take(result), Some(expr), ty_env, cx);
            gen_node(take(cont), ty_env, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_node(mut node: KNode, ty_env: &KTyEnv, cx: &mut Cx) {
    let KNode {
        prim,
        tys: _,
        ref mut args,
        ref mut results,
        ref mut conts,
        location: _,
    } = &mut node;
    match prim {
        KPrim::Stuck => unreachable!(),
        KPrim::Jump => match (args.as_mut_slice(), results.as_slice()) {
            ([KTerm::Return(_), KTerm::Unit { .. }], []) | ([KTerm::Return(_)], []) => {
                cx.stmts.push(CStmt::Return(None));
            }
            ([KTerm::Return(_), arg], []) => {
                let arg = gen_term(take(arg), cx);
                cx.stmts.push(CStmt::Return(Some(arg)));
            }
            ([KTerm::Label(label), args @ ..], []) => {
                let name = unique_label_name(*label, cx);
                let params = cx.labels[label.id()].params.to_owned();

                for (param, arg) in params.into_iter().zip(args) {
                    let name = unique_name(&param, cx);

                    if !cx.locals[param.local.id()].is_alive {
                        CStmt::Comment(format!("{} is skipped.", &name));
                        continue;
                    }

                    let arg = gen_term(take(arg), cx);

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
        KPrim::CallDirect => match (results.as_mut_slice(), conts.as_mut_slice()) {
            ([result], [cont]) => {
                let call_expr = {
                    let mut args = args.drain(..);
                    let left = gen_term(args.next().unwrap(), cx);
                    let args = args.map(|arg| gen_term(arg, cx));
                    left.into_call(args)
                };
                emit_var_decl(take(result), Some(call_expr), ty_env, cx);
                gen_node(take(cont), ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Let => match (
            args.as_mut_slice(),
            results.as_mut_slice(),
            conts.as_mut_slice(),
        ) {
            ([init], [result], [cont]) => {
                let init = gen_term(take(init), cx);
                emit_var_decl(take(result), Some(init), ty_env, cx);
                gen_node(take(cont), ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Struct => match (results.as_mut_slice(), conts.as_mut_slice()) {
            ([result], [cont]) => {
                let k_struct = result.ty(&cx.locals).as_struct().unwrap();

                let (name, ty) = gen_param(take(result), ty_env, cx);
                cx.stmts.push(CStmt::VarDecl {
                    name: name.clone(),
                    ty,
                    init_opt: None,
                });

                let outlines = cx.outlines.clone();
                for (arg, field) in args.iter_mut().zip(k_struct.fields(&outlines)) {
                    let left = CExpr::Name(name.clone()).into_dot(field.name(&outlines));
                    let right = gen_term(take(arg), cx);
                    cx.stmts
                        .push(left.into_binary_op(CBinaryOp::Assign, right).into_stmt());
                }

                gen_node(take(cont), ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::GetField => match (
            args.as_mut_slice(),
            results.as_mut_slice(),
            conts.as_mut_slice(),
        ) {
            (
                [left, KTerm::FieldTag(KFieldTag {
                    name: field_name, ..
                })],
                [result],
                [cont],
            ) => {
                let left = gen_term(take(left), cx);
                let expr = left.into_arrow(take(field_name)).into_ref();
                emit_var_decl(take(result), Some(expr), ty_env, cx);
                gen_node(take(cont), ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::If => match (
            args.as_mut_slice(),
            results.as_mut_slice(),
            conts.as_mut_slice(),
        ) {
            ([cond], [], [then_cont, else_cont]) => {
                let cond = gen_term(take(cond), cx);
                let then_cont = gen_node_as_block(take(then_cont), ty_env, cx);
                let else_cont = gen_node_as_block(take(else_cont), ty_env, cx);

                let body = Box::new(CStmt::Block(then_cont));
                let alt = Box::new(CStmt::Block(else_cont));
                cx.stmts.push(CStmt::If { cond, body, alt });
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => gen_unary_op(CUnaryOp::Deref, args, results, conts, ty_env, cx),
        KPrim::Ref => gen_unary_op(CUnaryOp::Ref, args, results, conts, ty_env, cx),
        KPrim::Minus => gen_unary_op(CUnaryOp::Minus, args, results, conts, ty_env, cx),
        KPrim::Not => gen_unary_op(CUnaryOp::Not, args, results, conts, ty_env, cx),
        KPrim::Add => gen_binary_op(CBinaryOp::Add, args, results, conts, ty_env, cx),
        KPrim::Sub => gen_binary_op(CBinaryOp::Sub, args, results, conts, ty_env, cx),
        KPrim::Mul => gen_binary_op(CBinaryOp::Mul, args, results, conts, ty_env, cx),
        KPrim::Div => gen_binary_op(CBinaryOp::Div, args, results, conts, ty_env, cx),
        KPrim::Modulo => gen_binary_op(CBinaryOp::Modulo, args, results, conts, ty_env, cx),
        KPrim::Eq => gen_binary_op(CBinaryOp::Eq, args, results, conts, ty_env, cx),
        KPrim::Ne => gen_binary_op(CBinaryOp::Ne, args, results, conts, ty_env, cx),
        KPrim::Lt => gen_binary_op(CBinaryOp::Lt, args, results, conts, ty_env, cx),
        KPrim::Le => gen_binary_op(CBinaryOp::Le, args, results, conts, ty_env, cx),
        KPrim::Gt => gen_binary_op(CBinaryOp::Gt, args, results, conts, ty_env, cx),
        KPrim::Ge => gen_binary_op(CBinaryOp::Ge, args, results, conts, ty_env, cx),
        KPrim::BitAnd => gen_binary_op(CBinaryOp::BitAnd, args, results, conts, ty_env, cx),
        KPrim::BitOr => gen_binary_op(CBinaryOp::BitOr, args, results, conts, ty_env, cx),
        KPrim::BitXor => gen_binary_op(CBinaryOp::BitXor, args, results, conts, ty_env, cx),
        KPrim::LeftShift => gen_binary_op(CBinaryOp::LeftShift, args, results, conts, ty_env, cx),
        KPrim::RightShift => gen_binary_op(CBinaryOp::RightShift, args, results, conts, ty_env, cx),
        KPrim::Cast => match (
            args.as_mut_slice(),
            results.as_mut_slice(),
            conts.as_mut_slice(),
        ) {
            ([arg], [result], [cont]) => {
                let arg = gen_term(take(arg), cx);
                let result_ty = gen_ty(result.ty(&cx.locals), ty_env, cx);
                let expr = arg.into_cast(result_ty);
                emit_var_decl(take(result), Some(expr), ty_env, cx);
                gen_node(take(cont), ty_env, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Assign => match (args.as_mut_slice(), conts.as_mut_slice()) {
            ([left, right], [cont]) => {
                match left {
                    KTerm::Name(symbol) if !cx.locals[symbol.local.id()].is_alive => {
                        cx.stmts.push(CStmt::Comment(format!(
                            "assignment to {} is eliminated.",
                            &cx.locals[symbol.local.id()].name
                        )));
                    }
                    _ => {
                        let left = gen_term(take(left), cx);
                        let right = gen_term(take(right), cx);
                        cx.stmts.push(
                            left.into_unary_op(CUnaryOp::Deref)
                                .into_binary_op(CBinaryOp::Assign, right)
                                .into_stmt(),
                        );
                    }
                }

                gen_node(take(cont), ty_env, cx);
            }
            _ => unimplemented!(),
        },
    }
}

fn gen_node_as_block(node: KNode, ty_env: &KTyEnv, cx: &mut Cx) -> CBlock {
    let stmts = cx.enter_block(|cx| gen_node(node, ty_env, cx));
    CBlock { stmts }
}

fn gen_fn_sig(
    params: Vec<KSymbol>,
    result_ty: KTy,
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

fn gen_root(root: KRoot, cx: &mut Cx) {
    let outlines = cx.outlines.clone();
    let empty_ty_env = KTyEnv::default();

    cx.fn_ident_ids.resize(outlines.fns.len(), None);
    cx.struct_ident_ids.resize(outlines.structs.len(), None);

    for k_struct in outlines.structs_iter() {
        let name = unique_struct_name(k_struct, cx);
        let fields = k_struct
            .fields(&outlines)
            .iter()
            .map(|field| {
                (
                    field.name(&cx.outlines).to_string(),
                    gen_ty(field.ty(&cx.outlines).clone(), &empty_ty_env, cx),
                )
            })
            .collect();
        cx.decls.push(CStmt::StructDecl { name, fields });
    }

    for (extern_fn, extern_fn_data) in outlines.extern_fns_iter().zip(root.extern_fns) {
        let KExternFnData { params, locals } = extern_fn_data;
        cx.locals = locals;
        cx.local_ident_ids.resize(cx.locals.len(), None);

        let name = unique_extern_fn_name(extern_fn, cx);
        let (params, result_ty) = {
            let result_ty = extern_fn.result_ty(&outlines).clone();
            gen_fn_sig(params, result_ty, &empty_ty_env, cx)
        };
        cx.decls.push(CStmt::ExternFnDecl {
            name,
            params,
            result_ty,
        });

        cx.locals.clear();
    }

    for (k_fn, fn_data) in outlines.fns_iter().zip(root.fns) {
        let KFnData {
            params,
            body,
            locals,
            labels,
            ty_env,
            ..
        } = fn_data;
        cx.locals = locals;
        cx.local_ident_ids.resize(cx.locals.len(), None);

        let stmts = cx.enter_block(|cx| {
            for id in 0..labels.len() {
                let label = KLabel::new(id);
                for i in 0..labels[label.id()].params.len() {
                    let param = labels[label.id()].params[i].clone();
                    emit_var_decl(param, None, &ty_env, cx);
                }
            }

            cx.labels = labels;
            cx.label_ident_ids.resize(cx.labels.len(), None);
            gen_node(body, &ty_env, cx);

            for id in 0..cx.labels.len() {
                let label = KLabel::new(id);
                let name = unique_label_name(label, cx);
                let body = take(&mut cx.labels[label.id()].body);
                cx.stmts.push(CStmt::Label { label: name });
                gen_node(body, &ty_env, cx);
            }
        });

        let name = unique_fn_name(k_fn, cx);
        let (params, result_ty) = {
            let result_ty = k_fn.result_ty(&outlines).clone();
            gen_fn_sig(params, result_ty, &empty_ty_env, cx)
        };
        cx.decls.push(CStmt::FnDecl {
            name,
            params,
            result_ty,
            body: CBlock { stmts },
        });

        cx.locals.clear();
    }
}

pub(crate) fn gen(k_root: KRoot, outlines: Rc<KOutlines>) -> CRoot {
    let mut cx = Cx::new(outlines);
    gen_root(k_root, &mut cx);
    CRoot { decls: cx.decls }
}
