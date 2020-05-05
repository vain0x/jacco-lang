use super::cps_fold::fold_block;
use super::flow::Flow;
use super::*;
use crate::parse::*;
use crate::token::TokenKind;

struct LoopConstruction {
    break_label: KSymbol,
    continue_label: KSymbol,
}

#[derive(Default)]
struct Gx {
    last_id: usize,
    current: Vec<XCommand>,
    parent_loop: Option<LoopConstruction>,
    extern_fns: Vec<KExternFn>,
    fns: Vec<KFn>,
}

impl Gx {
    fn name_to_symbol(&mut self, name: PName) -> KSymbol {
        KSymbol {
            id: name.name_id,
            text: name.text,
            location: name.location,
        }
    }

    fn fresh_symbol(&mut self, hint: &str, location: Location) -> KSymbol {
        self.last_id += 1;
        let id = self.last_id;

        let text = hint.to_string();

        KSymbol { id, text, location }
    }

    fn push(&mut self, command: XCommand) {
        self.current.push(command);
    }

    fn push_term(&mut self, term: KTerm) {
        self.push(XCommand::Term(term));
    }

    fn push_int(&mut self, value: i64, location: Location) {
        self.push_term(KTerm::Int(TokenData::new(
            TokenKind::Int,
            value.to_string(),
            location,
        )));
    }
}

fn emit_binary_op(
    prim: KPrim,
    left: PTerm,
    right_opt: Option<Box<PTerm>>,
    location: Location,
    gx: &mut Gx,
) {
    let result = gx.fresh_symbol(&prim.hint_str(), location.clone());

    gen_term_expr(left, gx);
    gen_term_expr(*right_opt.unwrap(), gx);

    gx.push(XCommand::Prim {
        prim,
        arg_count: 2,
        cont_count: 1,
        result,
        use_result: true,
        location,
    });
}

fn emit_if(
    cond: PTerm,
    gen_body: impl FnOnce(&mut Gx) -> Flow,
    gen_alt: impl FnOnce(&mut Gx) -> Flow,
    location: Location,
    gx: &mut Gx,
) -> Flow {
    let result = gx.fresh_symbol("cond", location.clone());
    let next_label = gx.fresh_symbol("next", location.clone());

    gen_term_expr(cond, gx);

    gx.push(XCommand::Prim {
        prim: KPrim::If,
        arg_count: 1,
        cont_count: 2,
        result,
        use_result: false,
        location,
    });

    // body:
    let body_flow = gen_body(gx);
    if body_flow == flow::SEQUENTIAL {
        gx.push(XCommand::Jump {
            label: next_label.clone(),
            arg_count: 0,
        });
    }

    // alt:
    let alt_flow = gen_alt(gx);
    if alt_flow == flow::SEQUENTIAL {
        gx.push(XCommand::Jump {
            label: next_label.clone(),
            arg_count: 0,
        });
    }

    // next:
    gx.push(XCommand::Label {
        label: next_label,
        arg_count: 0,
    });

    flow::join(body_flow, alt_flow)
}

fn gen_term_expr(term: PTerm, gx: &mut Gx) {
    match term {
        PTerm::Int(token) => {
            gx.push_term(KTerm::Int(token));
        }
        PTerm::Str(..) => unimplemented!(),
        PTerm::Name(name) => {
            let symbol = gx.name_to_symbol(name);
            gx.push_term(KTerm::Name(symbol));
        }
        PTerm::Call { callee, arg_list } => {
            let location = arg_list.left.into_location();
            let arg_count = arg_list.args.len() + 1;
            let result = gx.fresh_symbol("result", location.clone());

            gen_term_expr(*callee, gx);

            // FIXME: propagate flow?
            for arg in arg_list.args {
                gen_expr(arg.expr, gx);
            }

            gx.push(XCommand::Prim {
                prim: KPrim::CallDirect,
                arg_count,
                cont_count: 1,
                result: result.clone(),
                use_result: true,
                location,
            });
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
                let location1 = location.clone();
                let _ = emit_if(
                    *left,
                    |gx| {
                        gen_term_expr(*right_opt.unwrap(), gx);
                        flow::SEQUENTIAL
                    },
                    move |gx| {
                        gx.push_int(0, location);
                        flow::SEQUENTIAL
                    },
                    location1,
                    gx,
                );
            }
            BinaryOp::LogOr => {
                let location1 = location.clone();
                let _ = emit_if(
                    *left,
                    move |gx| {
                        gx.push_int(1, location);
                        flow::SEQUENTIAL
                    },
                    |gx| {
                        gen_term_expr(*right_opt.unwrap(), gx);
                        flow::SEQUENTIAL
                    },
                    location1,
                    gx,
                );
            }
        },
    }
}

fn gen_expr(expr: PExpr, gx: &mut Gx) -> Flow {
    match expr {
        PExpr::Term { term, .. } => {
            gen_term_expr(term, gx);
            flow::SEQUENTIAL
        }
        PExpr::Block(block) => gen_block(block, gx),
        PExpr::Break { .. } => {
            let dest = gx
                .parent_loop
                .as_ref()
                .expect("out of loop")
                .break_label
                .clone();
            gx.push(XCommand::Jump {
                label: dest,
                arg_count: 0,
            });
            flow::LOOP
        }
        PExpr::Continue { .. } => {
            let dest = gx
                .parent_loop
                .as_ref()
                .expect("out of loop")
                .continue_label
                .clone();
            gx.push(XCommand::Jump {
                label: dest,
                arg_count: 0,
            });
            flow::LOOP
        }
        PExpr::If {
            keyword,
            cond_opt,
            body_opt,
            alt_opt,
            ..
        } => {
            let location = keyword.into_location();
            emit_if(
                cond_opt.unwrap(),
                |gx| gen_block(body_opt.unwrap(), gx),
                |gx| {
                    let mut alt_flow = flow::SEQUENTIAL;
                    if let Some(alt) = alt_opt {
                        alt_flow = gen_expr(*alt, gx);
                    }
                    alt_flow
                },
                location,
                gx,
            )
        }
        PExpr::While {
            keyword,
            cond_opt,
            body_opt,
        } => {
            let location = keyword.into_location();
            let result = gx.fresh_symbol("cond", location.clone());
            let continue_label = gx.fresh_symbol("continue_", location.clone());
            let next_label = gx.fresh_symbol("next", location.clone());

            gx.push(XCommand::Jump {
                label: continue_label.clone(),
                arg_count: 0,
            });

            gx.push(XCommand::Label {
                label: continue_label.clone(),
                arg_count: 0,
            });

            gen_term_expr(cond_opt.unwrap(), gx);

            gx.push(XCommand::Prim {
                prim: KPrim::If,
                arg_count: 1,
                cont_count: 2,
                result,
                use_result: false,
                location,
            });

            let parent_loop = std::mem::replace(
                &mut gx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            let mut flow = gen_block(body_opt.unwrap(), gx);
            if flow == flow::SEQUENTIAL {
                gx.push(XCommand::Pop(1));

                gx.push(XCommand::Jump {
                    label: continue_label.clone(),
                    arg_count: 0,
                });
            }

            // alt:
            gx.push(XCommand::Jump {
                label: next_label.clone(),
                arg_count: 0,
            });

            gx.parent_loop = parent_loop;

            // next:
            gx.push(XCommand::Label {
                label: next_label,
                arg_count: 0,
            });

            gx.push_term(KTerm::Unit {
                location: Location::new_dummy(),
            });

            flow::end_loop(&mut flow);
            flow
        }
        PExpr::Loop { keyword, body_opt } => {
            let location = keyword.into_location();
            let continue_label = gx.fresh_symbol("continue_", location.clone());
            let next_label = gx.fresh_symbol("next", location.clone());

            gx.push(XCommand::Jump {
                label: continue_label.clone(),
                arg_count: 0,
            });

            gx.push(XCommand::Label {
                label: continue_label.clone(),
                arg_count: 0,
            });

            let parent_loop = std::mem::replace(
                &mut gx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            let mut flow = gen_block(body_opt.unwrap(), gx);
            if flow == flow::SEQUENTIAL {
                gx.push(XCommand::Pop(1));

                gx.push(XCommand::Jump {
                    label: continue_label.clone(),
                    arg_count: 0,
                });
            }

            gx.parent_loop = parent_loop;

            // next:
            gx.push(XCommand::Label {
                label: next_label,
                arg_count: 0,
            });

            gx.push_term(KTerm::Unit {
                location: Location::new_dummy(),
            });

            flow::end_loop(&mut flow);
            flow
        }
    }
}

fn gen_decl(decl: PDecl, gx: &mut Gx) -> Flow {
    match decl {
        PDecl::Expr(expr) => {
            let flow = gen_expr(expr, gx);
            gx.push(XCommand::Pop(1));
            flow
        }
        PDecl::Let {
            keyword,
            name_opt,
            init_opt,
        } => {
            let result = gx.name_to_symbol(name_opt.expect("missing name"));
            let location = keyword.into_location();

            let flow = gen_expr(init_opt.unwrap(), gx);

            gx.push(XCommand::Prim {
                prim: KPrim::Let,
                arg_count: 1,
                cont_count: 1,
                result,
                use_result: false,
                location,
            });

            flow
        }
        PDecl::Fn { keyword, block_opt } => {
            let location = keyword.into_location();
            let fn_name = gx.fresh_symbol("main", location.clone());
            let return_label = gx.fresh_symbol("return", location.clone());

            let commands = {
                let previous = std::mem::take(&mut gx.current);
                let parent_loop = std::mem::take(&mut gx.parent_loop);

                let arg_count = if block_opt.as_ref().unwrap().last_opt.is_some() {
                    1
                } else {
                    0
                };

                let flow = gen_block(block_opt.unwrap(), gx);
                if flow != flow::FN {
                    if arg_count == 0 {
                        gx.push(XCommand::Pop(1));
                    }

                    gx.push(XCommand::Jump {
                        label: return_label.clone(),
                        arg_count,
                    });
                }

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
            flow::SEQUENTIAL
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
            flow::SEQUENTIAL
        }
    }
}

fn gen_block(block: PBlock, gx: &mut Gx) -> Flow {
    for decl in block.decls {
        gen_decl(decl, gx)?;
    }

    if let Some(last) = block.last_opt {
        gen_expr(*last, gx)?;
    } else {
        let location = block.left.into_location();
        gx.push_term(KTerm::Unit { location });
    }

    flow::SEQUENTIAL
}

fn gen_root(root: PRoot, gx: &mut Gx) {
    for decl in root.decls {
        let flow = gen_decl(decl, gx);
        debug_assert_eq!(flow, flow::SEQUENTIAL);
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
