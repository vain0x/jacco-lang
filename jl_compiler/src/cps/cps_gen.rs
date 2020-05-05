//! 構文木から CPS ノードのもとになる命令列を生成する処理

use super::cps_fold::fold_block;
use super::*;
use crate::parse::*;
use crate::token::TokenKind;

struct LoopConstruction {
    break_label: KSymbol,
    continue_label: KSymbol,
}

/// Code generation context.
#[derive(Default)]
struct Gx {
    last_id: usize,
    current: Vec<XCommand>,
    parent_loop: Option<LoopConstruction>,
    extern_fns: Vec<KExternFn>,
    fns: Vec<KFn>,
}

impl Gx {
    fn fresh_id(&mut self) -> usize {
        self.last_id += 1;
        self.last_id
    }

    fn name_to_symbol(&mut self, name: PName) -> KSymbol {
        KSymbol {
            id: name.name_id,
            text: name.text,
            location: name.location,
        }
    }

    fn fresh_symbol(&mut self, hint: &str, location: Location) -> KSymbol {
        let id = self.fresh_id();

        let text = hint.to_string();

        KSymbol { id, text, location }
    }

    fn current_break_label(&self) -> Option<&KSymbol> {
        self.parent_loop.as_ref().map(|l| &l.break_label)
    }

    fn current_continue_label(&self) -> Option<&KSymbol> {
        self.parent_loop.as_ref().map(|l| &l.continue_label)
    }

    fn push(&mut self, command: XCommand) {
        self.current.push(command);
    }

    fn do_push_jump(
        &mut self,
        label: KSymbol,
        args: impl IntoIterator<Item = KTerm>,
        cont_count: usize,
    ) {
        self.push(XCommand::Prim {
            prim: KPrim::Jump,
            args: std::iter::once(KTerm::Name(label)).chain(args).collect(),
            result_opt: None,
            cont_count,
        });
    }

    fn push_jump(&mut self, label: KSymbol, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 0);
    }

    fn push_jump_with_cont(&mut self, label: KSymbol, args: impl IntoIterator<Item = KTerm>) {
        self.do_push_jump(label, args, 1);
    }
}

fn new_int_term(value: i64, location: Location) -> KTerm {
    KTerm::Int(TokenData::new(TokenKind::Int, value.to_string(), location))
}

fn new_false_term(location: Location) -> KTerm {
    new_int_term(0, location)
}

fn new_true_term(location: Location) -> KTerm {
    new_int_term(1, location)
}

fn new_unit_term(location: Location) -> KTerm {
    KTerm::Unit { location }
}

fn new_never_term(location: Location) -> KTerm {
    // FIXME: the type is ! (never)
    KTerm::Unit { location }
}

fn emit_binary_op(
    prim: KPrim,
    left: PTerm,
    right_opt: Option<Box<PTerm>>,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol(&prim.hint_str(), location.clone());

    let left = gen_term_expr(left, gx);
    let right = gen_term_expr(*right_opt.unwrap(), gx);

    gx.push(XCommand::Prim {
        prim,
        args: vec![left, right],
        result_opt: Some(result.clone()),
        cont_count: 1,
    });

    KTerm::Name(result)
}

fn emit_if(
    cond: PTerm,
    gen_body: impl FnOnce(&mut Gx) -> KTerm,
    gen_alt: impl FnOnce(&mut Gx) -> KTerm,
    location: Location,
    gx: &mut Gx,
) -> KTerm {
    let result = gx.fresh_symbol("if_result", location.clone());
    let next_label = gx.fresh_symbol("next", location.clone());

    let k_cond = gen_term_expr(cond, gx);

    gx.push(XCommand::Prim {
        prim: KPrim::If,
        args: vec![k_cond],
        cont_count: 2,
        result_opt: None,
    });

    // body
    {
        let body = gen_body(gx);
        gx.push_jump(next_label.clone(), vec![body]);
    }

    // alt
    {
        let alt = gen_alt(gx);
        gx.push_jump(next_label.clone(), vec![alt]);
    }

    gx.push(XCommand::Label {
        label: next_label,
        params: vec![result.clone()],
    });

    KTerm::Name(result)
}

fn gen_term_expr(term: PTerm, gx: &mut Gx) -> KTerm {
    match term {
        PTerm::Int(token) => return KTerm::Int(token),
        PTerm::Str(..) => unimplemented!(),
        PTerm::Name(name) => {
            let symbol = gx.name_to_symbol(name);
            KTerm::Name(symbol)
        }
        PTerm::Call { callee, arg_list } => {
            let location = arg_list.left.into_location();
            let result = gx.fresh_symbol("call_result", location.clone());

            let k_callee = gen_term_expr(*callee, gx);

            let mut k_args = vec![k_callee];
            for p_arg in arg_list.args {
                let k_arg = gen_expr(p_arg.expr, gx);
                k_args.push(k_arg);
            }

            gx.push(XCommand::Prim {
                prim: KPrim::CallDirect,
                args: k_args,
                result_opt: Some(result.clone()),
                cont_count: 1,
            });

            KTerm::Name(result)
        }
        PTerm::BinaryOp {
            op,
            left,
            right_opt,
            location,
        } => match op {
            BinaryOp::Add => emit_binary_op(KPrim::Add, *left, right_opt, location, gx),
            BinaryOp::Sub => emit_binary_op(KPrim::Sub, *left, right_opt, location, gx),
            BinaryOp::Mul => emit_binary_op(KPrim::Mul, *left, right_opt, location, gx),
            BinaryOp::Div => emit_binary_op(KPrim::Div, *left, right_opt, location, gx),
            BinaryOp::Mod => emit_binary_op(KPrim::Mod, *left, right_opt, location, gx),
            BinaryOp::BitAnd => emit_binary_op(KPrim::BitAnd, *left, right_opt, location, gx),
            BinaryOp::BitOr => emit_binary_op(KPrim::BitOr, *left, right_opt, location, gx),
            BinaryOp::BitXor => emit_binary_op(KPrim::BitXor, *left, right_opt, location, gx),
            BinaryOp::LeftShift => emit_binary_op(KPrim::LeftShift, *left, right_opt, location, gx),
            BinaryOp::RightShift => {
                emit_binary_op(KPrim::RightShift, *left, right_opt, location, gx)
            }
            BinaryOp::Eq => emit_binary_op(KPrim::Eq, *left, right_opt, location, gx),
            BinaryOp::Ne => emit_binary_op(KPrim::Ne, *left, right_opt, location, gx),
            BinaryOp::Lt => emit_binary_op(KPrim::Lt, *left, right_opt, location, gx),
            BinaryOp::Le => emit_binary_op(KPrim::Le, *left, right_opt, location, gx),
            BinaryOp::Gt => emit_binary_op(KPrim::Gt, *left, right_opt, location, gx),
            BinaryOp::Ge => emit_binary_op(KPrim::Ge, *left, right_opt, location, gx),
            BinaryOp::LogAnd => {
                let false_term = new_false_term(location.clone());
                emit_if(
                    *left,
                    |gx| gen_term_expr(*right_opt.unwrap(), gx),
                    move |_| false_term,
                    location,
                    gx,
                )
            }
            BinaryOp::LogOr => {
                let true_term = new_true_term(location.clone());
                emit_if(
                    *left,
                    move |_| true_term,
                    |gx| gen_term_expr(*right_opt.unwrap(), gx),
                    location,
                    gx,
                )
            }
        },
    }
}

fn gen_expr(expr: PExpr, gx: &mut Gx) -> KTerm {
    match expr {
        PExpr::Term { term, .. } => gen_term_expr(term, gx),
        PExpr::Block(block) => gen_block(block, gx),
        PExpr::Break { keyword, .. } => {
            let never_term = {
                let location = keyword.into_location();
                new_never_term(location)
            };

            let label = gx.current_break_label().expect("out of loop").clone();
            // FIXME: break is 1-arity
            gx.push_jump_with_cont(label, vec![]);

            never_term
        }
        PExpr::Continue { keyword, .. } => {
            let never_term = {
                let location = keyword.into_location();
                new_never_term(location)
            };

            let label = gx.current_continue_label().expect("out of loop").clone();
            gx.push_jump_with_cont(label, vec![]);

            never_term
        }
        PExpr::If {
            keyword,
            cond_opt,
            body_opt,
            alt_opt,
            ..
        } => {
            let location = keyword.into_location();
            let location1 = location.clone();
            emit_if(
                cond_opt.unwrap(),
                |gx| gen_block(body_opt.unwrap(), gx),
                move |gx| match alt_opt {
                    Some(alt) => gen_expr(*alt, gx),
                    None => new_unit_term(location),
                },
                location1,
                gx,
            )
        }
        PExpr::While {
            keyword,
            cond_opt,
            body_opt,
        } => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("while_result", location.clone());
            let continue_label = gx.fresh_symbol("continue_", location.clone());
            let next_label = gx.fresh_symbol("next", location.clone());

            gx.push_jump(continue_label.clone(), vec![]);

            gx.push(XCommand::Label {
                label: continue_label.clone(),
                params: vec![],
            });

            let k_cond = gen_term_expr(cond_opt.unwrap(), gx);

            gx.push(XCommand::Prim {
                prim: KPrim::If,
                args: vec![k_cond],
                result_opt: None,
                cont_count: 2,
            });

            let parent_loop = std::mem::replace(
                &mut gx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            gen_block(body_opt.unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![]);

            // alt:
            gx.push_jump(next_label.clone(), vec![new_unit_term(location)]);

            gx.parent_loop = parent_loop;

            // next:
            gx.push(XCommand::Label {
                label: next_label,
                params: vec![result.clone()],
            });

            KTerm::Name(result)
        }
        PExpr::Loop { keyword, body_opt } => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("loop_result", location.clone());
            let continue_label = gx.fresh_symbol("continue_", location.clone());
            let next_label = gx.fresh_symbol("next", location.clone());

            gx.push_jump(continue_label.clone(), vec![]);

            gx.push(XCommand::Label {
                label: continue_label.clone(),
                params: vec![],
            });

            let parent_loop = std::mem::replace(
                &mut gx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            gen_block(body_opt.unwrap(), gx);

            gx.push_jump(continue_label.clone(), vec![]);

            gx.parent_loop = parent_loop;

            // next:
            gx.push(XCommand::Label {
                label: next_label,
                params: vec![result.clone()],
            });

            KTerm::Name(result)
        }
    }
}

fn gen_decl(decl: PDecl, gx: &mut Gx) {
    match decl {
        PDecl::Expr(expr) => {
            gen_expr(expr, gx);
        }
        PDecl::Let {
            name_opt, init_opt, ..
        } => {
            let result = gx.name_to_symbol(name_opt.unwrap());

            let k_init = gen_expr(init_opt.unwrap(), gx);

            gx.push(XCommand::Prim {
                prim: KPrim::Let,
                args: vec![k_init],
                result_opt: Some(result),
                cont_count: 1,
            });
        }
        PDecl::Fn { keyword, block_opt } => {
            let location = keyword.into_location();
            let fn_name = gx.fresh_symbol("main", location.clone());
            let return_label = gx.fresh_symbol("return", location.clone());

            let commands = {
                let previous = std::mem::take(&mut gx.current);
                let parent_loop = std::mem::take(&mut gx.parent_loop);

                let k_result = gen_block(block_opt.unwrap(), gx);

                gx.push_jump(return_label.clone(), vec![k_result]);

                gx.parent_loop = parent_loop;
                std::mem::replace(&mut gx.current, previous)
            };

            let (node, labels) = fold_block(commands);

            let k_fn = KFn {
                name: fn_name,
                params: vec![return_label],
                body: node,
                labels,
            };

            gx.fns.push(k_fn);
        }
        PDecl::ExternFn {
            name_opt,
            param_list_opt,
            result_opt,
            ..
        } => {
            let fn_name = gx.name_to_symbol(name_opt.unwrap());

            let result = match result_opt {
                Some(result) => match result.ty_opt.unwrap().text.as_str() {
                    "i32" => KTy::I32,
                    _ => unimplemented!(),
                },
                None => KTy::Unit,
            };

            let extern_fn = KExternFn {
                name: fn_name,
                params: param_list_opt
                    .unwrap()
                    .params
                    .into_iter()
                    .map(|param| (gx.name_to_symbol(param.name), KTy::I32))
                    .collect(),
                result,
            };

            gx.extern_fns.push(extern_fn);
        }
    }
}

fn gen_block(block: PBlock, gx: &mut Gx) -> KTerm {
    for decl in block.decls {
        gen_decl(decl, gx);
    }

    if let Some(last) = block.last_opt {
        gen_expr(*last, gx)
    } else {
        let location = block.left.into_location();
        KTerm::Unit { location }
    }
}

fn gen_root(root: PRoot, gx: &mut Gx) {
    for decl in root.decls {
        gen_decl(decl, gx);
    }
}

pub(crate) fn cps_conversion(p_root: PRoot) -> KRoot {
    let mut gx = Gx::default();
    gen_root(p_root, &mut gx);

    KRoot {
        extern_fns: gx.extern_fns,
        fns: gx.fns,
    }
}
