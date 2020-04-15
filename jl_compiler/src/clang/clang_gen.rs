use super::*;
use crate::cps::*;
use crate::token::TokenData;
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

fn gen_term(term: KTerm, _cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Int(token) => CExpr::IntLit(token.into_text()),
        KTerm::Name { text, .. } => CExpr::Name(text),
    }
}

fn gen_binary_op(
    op: CBinaryOp,
    args: &mut Vec<KTerm>,
    results: &mut Vec<String>,
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
                name: mem::take(result),
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
            KPrim::Add => gen_binary_op(CBinaryOp::Add, args, results, conts, stmts, cx),
            KPrim::Sub => gen_binary_op(CBinaryOp::Sub, args, results, conts, stmts, cx),
            KPrim::Mul => gen_binary_op(CBinaryOp::Mul, args, results, conts, stmts, cx),
            KPrim::Div => gen_binary_op(CBinaryOp::Div, args, results, conts, stmts, cx),
            KPrim::Mod => gen_binary_op(CBinaryOp::Mod, args, results, conts, stmts, cx),
        },
        KNode::Jump { ref mut args, .. } => match args.as_mut_slice() {
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
    for KFn { name, body, .. } in root.fns {
        let body = gen_node_as_block(body, cx);

        cx.decls.push(CStmt::FnDecl {
            ident: name,
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
