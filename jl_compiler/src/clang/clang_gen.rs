use super::*;
use crate::cps::*;
use crate::token::TokenData;

#[derive(Default)]
struct Cx {
    decls: Vec<CStmt>,
}

fn take_term(slot: &mut KTerm) -> KTerm {
    std::mem::replace(slot, KTerm::Int(TokenData::new_dummy()))
}

fn gen_term(term: KTerm, cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Int(token) => CExpr::IntLit(token.into_text()),
        KTerm::Name(token) => CExpr::Name(token.into_text()),
    }
}

fn gen_node_as_block(mut node: KNode, cx: &mut Cx) -> CBlock {
    let mut stmts = vec![];

    match node {
        KNode::Jump { ref mut args, .. } => match args.as_mut_slice() {
            [arg] => {
                let arg = gen_term(take_term(arg), cx);
                stmts.push(CStmt::Return(Some(arg)));
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }

    CBlock { body: stmts }
}

fn gen_root(root: KRoot, cx: &mut Cx) {
    for KFn { name, params, body } in root.fns {
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
