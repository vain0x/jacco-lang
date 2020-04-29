use super::*;
use std::mem;

#[derive(Default)]
struct Cx {
    decls: Vec<CStmt>,
}

fn take_term(slot: &mut KTerm) -> KTerm {
    mem::replace(slot, KTerm::Int(TokenData::new_dummy()))
}

fn take_node(slot: &mut KNode) -> KNode {
    mem::replace(slot, KNode::Abort)
}

fn gen_ty(ty: KTy) -> CTy {
    match ty {
        KTy::Unit => CTy::Void,
        KTy::I32 => CTy::Int,
    }
}

fn gen_term(term: KTerm, _cx: &mut Cx) -> CExpr {
    match term {
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
            let left = gen_term(take_term(left), cx);
            let right = gen_term(take_term(right), cx);
            stmts.push(CStmt::VarDecl {
                name: mem::take(result).unique_name(),
                ty: CTy::Int,
                init_opt: Some(CExpr::BinaryOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                }),
            });
            gen_node(take_node(cont), stmts, cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_node(mut node: KNode, stmts: &mut Vec<CStmt>, cx: &mut Cx) {
    match node {
        KNode::Prim {
            prim,
            ref mut args,
            ref mut results,
            ref mut conts,
        } => match prim {
            KPrim::Let => match (
                args.as_mut_slice(),
                results.as_mut_slice(),
                conts.as_mut_slice(),
            ) {
                ([init], [result], [cont]) => {
                    let init = gen_term(take_term(init), cx);
                    stmts.push(CStmt::VarDecl {
                        name: mem::take(result).unique_name(),
                        ty: CTy::Int,
                        init_opt: Some(init),
                    });
                    gen_node(take_node(cont), stmts, cx);
                }
                _ => unimplemented!(),
            },
            KPrim::If => match (
                args.as_mut_slice(),
                results.as_mut_slice(),
                conts.as_mut_slice(),
            ) {
                ([cond], [_result], [cont]) => {
                    let cond = gen_term(take_term(cond), cx);

                    let body = Box::new(CStmt::Block(CBlock { body: vec![] }));
                    stmts.push(CStmt::If { cond, body });

                    gen_node(take_node(cont), stmts, cx);
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
        },
        KNode::Jump {
            ref mut label,
            ref mut args,
        } => match args.as_mut_slice() {
            [] => {
                stmts.push(CStmt::Expr(CExpr::Call {
                    cal: Box::new(CExpr::Name(mem::take(label).unique_name())),
                    args: vec![],
                }));
            }
            [arg] => {
                let arg = gen_term(take_term(arg), cx);
                stmts.push(CStmt::Return(Some(arg)));
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
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

    for KFn { name, body, .. } in root.fns {
        let body = gen_node_as_block(body, cx);

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
