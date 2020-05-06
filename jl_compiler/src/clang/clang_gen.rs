//! CPS 中間表現をC言語のコードに変換する処理

use super::*;
use std::collections::HashMap;
use std::mem::take;

/// C code generation context.
#[derive(Default)]
struct Cx {
    labels: HashMap<String, Vec<KSymbol>>,
    decls: Vec<CStmt>,
}

fn gen_ty(ty: KTy) -> CTy {
    match ty {
        KTy::Unit => CTy::Void,
        KTy::I32 => CTy::Int,
    }
}

fn gen_term(term: KTerm, _cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Unit { .. } => CExpr::IntLit("(void)0".to_string()),
        KTerm::Int(token) => CExpr::IntLit(token.into_text()),
        KTerm::Name(symbol) => CExpr::Name(symbol.unique_name()),
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
            stmts.push(CStmt::VarDecl {
                name: take(result).unique_name(),
                ty: CTy::Int,
                init_opt: Some(CExpr::BinaryOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }),
            });
            gen_node(take(cont), stmts, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_node(mut node: KNode, stmts: &mut Vec<CStmt>, cx: &mut Cx) {
    let KNode {
        prim,
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
                let name = take(label).unique_name();
                let params = cx
                    .labels
                    .get(&name)
                    .into_iter()
                    .flatten()
                    .map(KSymbol::unique_name)
                    .collect::<Vec<_>>();

                for (param, arg) in params.into_iter().zip(args) {
                    let arg = gen_term(take(arg), cx);

                    stmts.push(CStmt::Expr(CExpr::BinaryOp {
                        op: CBinaryOp::Assign,
                        left: Box::new(CExpr::Name(param)),
                        right: Box::new(arg),
                    }))
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

                let let_stmt = CStmt::VarDecl {
                    name: take(result).unique_name(),
                    ty: CTy::Int,
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
                stmts.push(CStmt::VarDecl {
                    name: take(result).unique_name(),
                    ty: CTy::Int,
                    init_opt: Some(init),
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
    }
}

fn gen_node_as_block(node: KNode, cx: &mut Cx) -> CBlock {
    let mut stmts = vec![];

    gen_node(node, &mut stmts, cx);

    CBlock { body: stmts }
}

fn gen_root(root: KRoot, cx: &mut Cx) {
    for KExternFn {
        name,
        params,
        result,
    } in root.extern_fns
    {
        cx.decls.push(CStmt::ExternFnDecl {
            name: name.text,
            params: params
                .into_iter()
                .map(|(name, ty)| (name.unique_name(), gen_ty(ty)))
                .collect(),
            result_ty: gen_ty(result),
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
                cx.labels.insert(name.unique_name(), params.clone());

                for param in params {
                    stmts.push(CStmt::VarDecl {
                        name: param.unique_name(),
                        ty: CTy::Int,
                        init_opt: None,
                    });
                }
            }

            gen_node(body, &mut stmts, cx);

            for KFn { name, body, .. } in labels {
                stmts.push(CStmt::Label {
                    label: name.unique_name(),
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
