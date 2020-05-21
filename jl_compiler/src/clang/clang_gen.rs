//! CPS 中間表現をC言語のコードに変換する処理

use super::*;
use std::collections::HashMap;
use std::mem::take;

/// C code generation context.
#[derive(Default)]
struct Cx {
    ids: IdProvider,
    labels: HashMap<String, Vec<String>>,
    decls: Vec<CStmt>,
}

fn gen_ty(ty: KTy, ids: &mut IdProvider) -> CTy {
    match ty {
        KTy::Unresolved => CTy::Other("/* unresolved */ void"),
        KTy::Meta(meta) => match meta.try_resolve() {
            Some(ty) => gen_ty(ty, ids),
            None => CTy::Other("/* free */ void"),
        },
        KTy::Never => CTy::Other("/* never */ void"),
        KTy::Fn { .. } => CTy::Other("/* fn */ void"),
        KTy::Unit => CTy::Void,
        KTy::I32 => CTy::Int,
        KTy::Ptr { ty } => gen_ty(*ty, ids).into_ptr(),
        KTy::Symbol { def } => CTy::Struct(def.borrow().name.unique_name(ids)),
    }
}

fn gen_param(param: KSymbol, cx: &mut Cx) -> (String, CTy) {
    (
        param.unique_name(&mut cx.ids),
        gen_ty(param.ty, &mut cx.ids),
    )
}

fn gen_term(term: KTerm, cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Unit { .. } => CExpr::IntLit("(void)0".to_string()),
        KTerm::Int(token) => CExpr::IntLit(token.into_text()),
        KTerm::Name(symbol) => CExpr::Name(symbol.unique_name(&mut cx.ids)),
        KTerm::Field { text, location } => {
            error!("can't gen field term to c {} ({:?})", text, location);
            CExpr::IntLit("0".to_string())
        }
    }
}

fn gen_unary_op(
    op: CUnaryOp,
    args: &mut Vec<KTerm>,
    results: &mut Vec<KSymbol>,
    conts: &mut Vec<KNode>,
    stmts: &mut Vec<CStmt>,
    cx: &mut Cx,
) {
    match (
        args.as_mut_slice(),
        results.as_mut_slice(),
        conts.as_mut_slice(),
    ) {
        ([arg], [result], [cont]) => {
            let arg = gen_term(take(arg), cx);
            let (name, ty) = gen_param(take(result), cx);
            stmts.push(CStmt::VarDecl {
                name,
                ty,
                init_opt: Some(arg.into_unary_op(op)),
            });
            gen_node(take(cont), stmts, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_binary_op(
    op: CBinaryOp,
    args: &mut Vec<KTerm>,
    results: &mut Vec<KSymbol>,
    conts: &mut Vec<KNode>,
    stmts: &mut Vec<CStmt>,
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
            let (name, ty) = gen_param(take(result), cx);
            stmts.push(CStmt::VarDecl {
                name,
                ty,
                init_opt: Some(left.into_binary_op(op, right)),
            });
            gen_node(take(cont), stmts, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_node(mut node: KNode, stmts: &mut Vec<CStmt>, cx: &mut Cx) {
    let KNode {
        prim,
        tys: _,
        ref mut args,
        ref mut results,
        ref mut conts,
    } = &mut node;
    match prim {
        KPrim::Stuck => unreachable!(),
        KPrim::Jump => match (args.as_mut_slice(), results.as_slice()) {
            ([KTerm::Name(label), arg], []) if label.text == "return" => {
                let arg = gen_term(take(arg), cx);
                stmts.push(CStmt::Return(Some(arg)));
            }
            ([KTerm::Name(label)], []) if label.text == "return" => {
                stmts.push(CStmt::Return(None));
            }
            ([KTerm::Name(label), args @ ..], []) => {
                let name = take(label).unique_name(&mut cx.ids);
                let params = cx
                    .labels
                    .get(&name)
                    .into_iter()
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>();

                for (param, arg) in params.into_iter().zip(args) {
                    let arg = gen_term(take(arg), cx);

                    stmts.push(
                        CExpr::Name(param)
                            .into_binary_op(CBinaryOp::Assign, arg)
                            .into_stmt(),
                    );
                }

                stmts.push(CStmt::Goto { label: name });
            }
            _ => unimplemented!(),
        },
        KPrim::CallDirect => match (results.as_mut_slice(), conts.as_mut_slice()) {
            ([result], [cont]) => {
                let call_expr = {
                    let cal = Box::new(gen_term(args.remove(0), cx));
                    let args = args.drain(..).map(|arg| gen_term(arg, cx)).collect();
                    CExpr::Call { cal, args }
                };
                let (name, ty) = gen_param(take(result), cx);

                let let_stmt = CStmt::VarDecl {
                    name,
                    ty,
                    init_opt: Some(call_expr),
                };

                stmts.push(let_stmt);

                gen_node(take(cont), stmts, cx);
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
                let (name, ty) = gen_param(take(result), cx);
                stmts.push(CStmt::VarDecl {
                    name,
                    ty,
                    init_opt: Some(init),
                });
                gen_node(take(cont), stmts, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::Struct => match (results.as_mut_slice(), conts.as_mut_slice()) {
            ([result], [cont]) => {
                let struct_def = match result.ty.clone().resolve() {
                    KTy::Symbol { def } => def.clone(),
                    _ => unimplemented!(),
                };

                let (name, ty) = gen_param(take(result), cx);
                stmts.push(CStmt::VarDecl {
                    name: name.clone(),
                    ty,
                    init_opt: None,
                });

                for (arg, field) in args.iter_mut().zip(&struct_def.borrow().fields) {
                    let field_name = field.name.unique_name(&mut cx.ids);

                    let left = CExpr::Name(name.clone()).into_dot(field_name);
                    let right = gen_term(take(arg), cx);
                    stmts.push(left.into_binary_op(CBinaryOp::Assign, right).into_stmt());
                }

                gen_node(take(cont), stmts, cx);
            }
            _ => unimplemented!(),
        },
        KPrim::GetField => match (
            args.as_mut_slice(),
            results.as_mut_slice(),
            conts.as_mut_slice(),
        ) {
            (
                [left, KTerm::Field {
                    text: field_name, ..
                }],
                [result],
                [cont],
            ) => {
                let left = gen_term(take(left), cx);
                let (name, ty) = gen_param(take(result), cx);

                stmts.push(CStmt::VarDecl {
                    name,
                    ty,
                    init_opt: Some(left.into_arrow(take(field_name)).into_ref()),
                });

                gen_node(take(cont), stmts, cx);
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
                let then_cont = gen_node_as_block(take(then_cont), cx);
                let else_cont = gen_node_as_block(take(else_cont), cx);

                let body = Box::new(CStmt::Block(then_cont));
                let alt = Box::new(CStmt::Block(else_cont));
                stmts.push(CStmt::If { cond, body, alt });
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => gen_unary_op(CUnaryOp::Deref, args, results, conts, stmts, cx),
        KPrim::Ref => gen_unary_op(CUnaryOp::Ref, args, results, conts, stmts, cx),
        KPrim::Minus => gen_unary_op(CUnaryOp::Minus, args, results, conts, stmts, cx),
        KPrim::Negate => gen_unary_op(CUnaryOp::Negate, args, results, conts, stmts, cx),
        KPrim::Add => gen_binary_op(CBinaryOp::Add, args, results, conts, stmts, cx),
        KPrim::Sub => gen_binary_op(CBinaryOp::Sub, args, results, conts, stmts, cx),
        KPrim::Mul => gen_binary_op(CBinaryOp::Mul, args, results, conts, stmts, cx),
        KPrim::Div => gen_binary_op(CBinaryOp::Div, args, results, conts, stmts, cx),
        KPrim::Mod => gen_binary_op(CBinaryOp::Mod, args, results, conts, stmts, cx),
        KPrim::Eq => gen_binary_op(CBinaryOp::Eq, args, results, conts, stmts, cx),
        KPrim::Ne => gen_binary_op(CBinaryOp::Ne, args, results, conts, stmts, cx),
        KPrim::Lt => gen_binary_op(CBinaryOp::Lt, args, results, conts, stmts, cx),
        KPrim::Le => gen_binary_op(CBinaryOp::Le, args, results, conts, stmts, cx),
        KPrim::Gt => gen_binary_op(CBinaryOp::Gt, args, results, conts, stmts, cx),
        KPrim::Ge => gen_binary_op(CBinaryOp::Ge, args, results, conts, stmts, cx),
        KPrim::BitAnd => gen_binary_op(CBinaryOp::BitAnd, args, results, conts, stmts, cx),
        KPrim::BitOr => gen_binary_op(CBinaryOp::BitOr, args, results, conts, stmts, cx),
        KPrim::BitXor => gen_binary_op(CBinaryOp::BitXor, args, results, conts, stmts, cx),
        KPrim::LeftShift => gen_binary_op(CBinaryOp::LeftShift, args, results, conts, stmts, cx),
        KPrim::RightShift => gen_binary_op(CBinaryOp::RightShift, args, results, conts, stmts, cx),
        KPrim::Assign => match (
            args.as_mut_slice(),
            results.as_mut_slice(),
            conts.as_mut_slice(),
        ) {
            ([left, right], [_result], [cont]) => {
                let left = gen_term(take(left), cx);
                let right = gen_term(take(right), cx);
                stmts.push(
                    left.into_unary_op(CUnaryOp::Deref)
                        .into_binary_op(CBinaryOp::Assign, right)
                        .into_stmt(),
                );
                gen_node(take(cont), stmts, cx);
            }
            _ => unimplemented!(),
        },
    }
}

fn gen_node_as_block(node: KNode, cx: &mut Cx) -> CBlock {
    let mut stmts = vec![];

    gen_node(node, &mut stmts, cx);

    CBlock { body: stmts }
}

fn gen_root(root: KRoot, cx: &mut Cx) {
    for KStruct { def } in root.structs {
        let name = def.borrow().name.unique_name(&mut cx.ids);
        let fields = def
            .borrow()
            .fields
            .iter()
            .map(|field| gen_param(field.name.clone(), cx))
            .collect();
        cx.decls.push(CStmt::StructDecl { name, fields });
    }

    for KExternFn {
        name,
        params,
        result,
    } in root.extern_fns
    {
        let params = params
            .into_iter()
            .map(|param| gen_param(param, cx))
            .collect();
        cx.decls.push(CStmt::ExternFnDecl {
            name: name.text,
            params,
            result_ty: gen_ty(result, &mut cx.ids),
        });
    }

    for KFn {
        name, body, labels, ..
    } in root.fns
    {
        let body = {
            let mut stmts = vec![];

            cx.labels.clear();
            for KFn { name, params, .. } in &labels {
                let fn_name = name.unique_name(&mut cx.ids);
                let params = params
                    .into_iter()
                    .map(|param| param.unique_name(&mut cx.ids))
                    .collect::<Vec<_>>();
                cx.labels.insert(fn_name, params.clone());

                for name in params {
                    stmts.push(CStmt::VarDecl {
                        name,
                        ty: CTy::Int,
                        init_opt: None,
                    });
                }
            }

            gen_node(body, &mut stmts, cx);

            for KFn { name, body, .. } in labels {
                stmts.push(CStmt::Label {
                    label: name.unique_name(&mut cx.ids),
                });
                gen_node(body, &mut stmts, cx);
            }

            CBlock { body: stmts }
        };

        cx.decls.push(CStmt::FnDecl {
            name: name.text,
            params: vec![],
            result_ty: CTy::Int,
            body,
        });
    }
}

pub(crate) fn gen(k_root: KRoot) -> CRoot {
    let mut cx = Cx::default();
    gen_root(k_root, &mut cx);
    CRoot { body: cx.decls }
}
