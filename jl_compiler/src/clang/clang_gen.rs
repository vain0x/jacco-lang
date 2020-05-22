//! CPS 中間表現をC言語のコードに変換する処理

use super::*;
use std::collections::HashMap;
use std::mem::{replace, take};

/// C code generation context.
#[derive(Default)]
struct Cx {
    name_map: HashMap<String, IdProvider>,
    labels: HashMap<String, Vec<String>>,
    stmts: Vec<CStmt>,
    decls: Vec<CStmt>,
}

impl Cx {
    fn ident_id(&mut self, name: &str) -> usize {
        let ids = match self.name_map.get_mut(name) {
            Some(ids) => ids,
            None => {
                self.name_map
                    .insert(name.to_string(), IdProvider::default());
                self.name_map.get_mut(name).unwrap()
            }
        };

        ids.next()
    }

    fn enter_block(&mut self, gen_fn: impl FnOnce(&mut Self)) -> Vec<CStmt> {
        let stmts = take(&mut self.stmts);

        gen_fn(self);

        replace(&mut self.stmts, stmts)
    }
}

fn unique_name(symbol: &KSymbol, cx: &mut Cx) -> String {
    let id_opt = symbol.def.id_opt.borrow().as_ref().cloned();
    let id = id_opt.unwrap_or_else(|| cx.ident_id(&symbol.text));
    *symbol.def.id_opt.borrow_mut() = Some(id);

    format!("{}_{:x}", symbol.text, id)
}

fn gen_ty(ty: KTy, cx: &mut Cx) -> CTy {
    match ty {
        KTy::Unresolved => {
            error!("Unexpected unresolved type {:?}", ty);
            CTy::Other("/* unresolved */ void")
        }
        KTy::Meta(meta) => match meta.try_resolve() {
            Some(ty) => gen_ty(ty, cx),
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
        KTy::I32 => CTy::Int,
        KTy::Ptr { ty } => gen_ty(*ty, cx).into_ptr(),
        KTy::Symbol { def } => CTy::Struct(unique_name(&def.borrow().name, cx)),
    }
}

fn gen_param(param: KSymbol, cx: &mut Cx) -> (String, CTy) {
    (unique_name(&param, cx), gen_ty(param.ty, cx))
}

fn gen_term(term: KTerm, cx: &mut Cx) -> CExpr {
    match term {
        KTerm::Unit { .. } => {
            // FIXME: error!
            CExpr::IntLit("(void)0".to_string())
        }
        KTerm::Int(token) => CExpr::IntLit(token.into_text()),
        KTerm::Name(symbol) => CExpr::Name(unique_name(&symbol, cx)),
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
            cx.stmts.push(CStmt::VarDecl {
                name,
                ty,
                init_opt: Some(arg.into_unary_op(op)),
            });
            gen_node(take(cont), cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_binary_op(
    op: CBinaryOp,
    args: &mut Vec<KTerm>,
    results: &mut Vec<KSymbol>,
    conts: &mut Vec<KNode>,
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
            cx.stmts.push(CStmt::VarDecl {
                name,
                ty,
                init_opt: Some(left.into_binary_op(op, right)),
            });
            gen_node(take(cont), cx);
        }
        _ => unimplemented!(),
    }
}

fn gen_node(mut node: KNode, cx: &mut Cx) {
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
                cx.stmts.push(CStmt::Return(Some(arg)));
            }
            ([KTerm::Name(label)], []) if label.text == "return" => {
                cx.stmts.push(CStmt::Return(None));
            }
            ([KTerm::Name(label), args @ ..], []) => {
                let name = unique_name(&label, cx);
                let params = cx
                    .labels
                    .get(&name)
                    .into_iter()
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>();

                for (param, arg) in params.into_iter().zip(args) {
                    let arg = gen_term(take(arg), cx);

                    cx.stmts.push(
                        CExpr::Name(param)
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
                let (name, ty) = gen_param(take(result), cx);

                let let_stmt = CStmt::VarDecl {
                    name,
                    ty,
                    init_opt: Some(call_expr),
                };

                cx.stmts.push(let_stmt);

                gen_node(take(cont), cx);
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
                cx.stmts.push(CStmt::VarDecl {
                    name,
                    ty,
                    init_opt: Some(init),
                });
                gen_node(take(cont), cx);
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
                cx.stmts.push(CStmt::VarDecl {
                    name: name.clone(),
                    ty,
                    init_opt: None,
                });

                for (arg, field) in args.iter_mut().zip(&struct_def.borrow().fields) {
                    let field_name = unique_name(&field.name, cx);

                    let left = CExpr::Name(name.clone()).into_dot(field_name);
                    let right = gen_term(take(arg), cx);
                    cx.stmts
                        .push(left.into_binary_op(CBinaryOp::Assign, right).into_stmt());
                }

                gen_node(take(cont), cx);
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

                cx.stmts.push(CStmt::VarDecl {
                    name,
                    ty,
                    init_opt: Some(left.into_arrow(take(field_name)).into_ref()),
                });

                gen_node(take(cont), cx);
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
                cx.stmts.push(CStmt::If { cond, body, alt });
            }
            _ => unimplemented!(),
        },
        KPrim::Deref => gen_unary_op(CUnaryOp::Deref, args, results, conts, cx),
        KPrim::Ref => gen_unary_op(CUnaryOp::Ref, args, results, conts, cx),
        KPrim::Minus => gen_unary_op(CUnaryOp::Minus, args, results, conts, cx),
        KPrim::Not => gen_unary_op(CUnaryOp::Not, args, results, conts, cx),
        KPrim::Add => gen_binary_op(CBinaryOp::Add, args, results, conts, cx),
        KPrim::Sub => gen_binary_op(CBinaryOp::Sub, args, results, conts, cx),
        KPrim::Mul => gen_binary_op(CBinaryOp::Mul, args, results, conts, cx),
        KPrim::Div => gen_binary_op(CBinaryOp::Div, args, results, conts, cx),
        KPrim::Modulo => gen_binary_op(CBinaryOp::Modulo, args, results, conts, cx),
        KPrim::Eq => gen_binary_op(CBinaryOp::Eq, args, results, conts, cx),
        KPrim::Ne => gen_binary_op(CBinaryOp::Ne, args, results, conts, cx),
        KPrim::Lt => gen_binary_op(CBinaryOp::Lt, args, results, conts, cx),
        KPrim::Le => gen_binary_op(CBinaryOp::Le, args, results, conts, cx),
        KPrim::Gt => gen_binary_op(CBinaryOp::Gt, args, results, conts, cx),
        KPrim::Ge => gen_binary_op(CBinaryOp::Ge, args, results, conts, cx),
        KPrim::BitAnd => gen_binary_op(CBinaryOp::BitAnd, args, results, conts, cx),
        KPrim::BitOr => gen_binary_op(CBinaryOp::BitOr, args, results, conts, cx),
        KPrim::BitXor => gen_binary_op(CBinaryOp::BitXor, args, results, conts, cx),
        KPrim::LeftShift => gen_binary_op(CBinaryOp::LeftShift, args, results, conts, cx),
        KPrim::RightShift => gen_binary_op(CBinaryOp::RightShift, args, results, conts, cx),
        KPrim::Assign => match (args.as_mut_slice(), conts.as_mut_slice()) {
            ([left, right], [cont]) => {
                let left = gen_term(take(left), cx);
                let right = gen_term(take(right), cx);
                cx.stmts.push(
                    left.into_unary_op(CUnaryOp::Deref)
                        .into_binary_op(CBinaryOp::Assign, right)
                        .into_stmt(),
                );
                gen_node(take(cont), cx);
            }
            _ => unimplemented!(),
        },
    }
}

fn gen_node_as_block(node: KNode, cx: &mut Cx) -> CBlock {
    let stmts = cx.enter_block(|cx| gen_node(node, cx));
    CBlock { stmts }
}

fn gen_root(root: KRoot, cx: &mut Cx) {
    for KStruct { def } in root.structs {
        let name = unique_name(&def.borrow().name, cx);
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
        result_ty,
    } in root.extern_fns
    {
        let params = params
            .into_iter()
            .map(|param| gen_param(param, cx))
            .collect();
        let result_ty = gen_ty(result_ty, cx);
        cx.decls.push(CStmt::ExternFnDecl {
            name: name.text,
            params,
            result_ty,
        });
    }

    for KFn {
        name, body, labels, ..
    } in root.fns
    {
        let stmts = cx.enter_block(|cx| {
            cx.labels.clear();
            for KFn { name, params, .. } in &labels {
                let fn_name = unique_name(&name, cx);
                let params = params
                    .into_iter()
                    .map(|param| unique_name(&param, cx))
                    .collect::<Vec<_>>();
                cx.labels.insert(fn_name, params.clone());

                for name in params {
                    cx.stmts.push(CStmt::VarDecl {
                        name,
                        ty: CTy::Int,
                        init_opt: None,
                    });
                }
            }

            gen_node(body, cx);

            for KFn { name, body, .. } in labels {
                let label = unique_name(&name, cx);
                cx.stmts.push(CStmt::Label { label });
                gen_node(body, cx);
            }
        });

        cx.decls.push(CStmt::FnDecl {
            name: name.text,
            params: vec![],
            result_ty: CTy::Int,
            body: CBlock { stmts },
        });
    }
}

pub(crate) fn gen(k_root: KRoot) -> CRoot {
    let mut cx = Cx::default();
    gen_root(k_root, &mut cx);
    CRoot { decls: cx.decls }
}
