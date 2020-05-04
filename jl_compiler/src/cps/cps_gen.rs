use self::flow::Flow;
use super::*;
use crate::parse::*;
use crate::token::TokenKind;

mod flow {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(crate) enum JumpTarget {
        /// break or continue
        Loop,
        /// return
        Fn,
    }

    pub(crate) type Flow = Result<(), JumpTarget>;

    pub(crate) const SEQUENTIAL: Flow = Ok(());

    pub(crate) const LOOP: Flow = Err(JumpTarget::Loop);

    pub(crate) const FN: Flow = Err(JumpTarget::Fn);

    pub(crate) fn end_loop(flow: &mut Flow) {
        if *flow == LOOP {
            *flow = SEQUENTIAL;
        }
    }

    pub(crate) fn join(first: Flow, second: Flow) -> Flow {
        match (first, second) {
            (SEQUENTIAL, _) | (_, SEQUENTIAL) => SEQUENTIAL,
            (LOOP, _) | (_, LOOP) => LOOP,
            (FN, FN) => FN,
        }
    }
}

#[derive(Debug)]
enum XCommand {
    Pop(usize),
    Term(KTerm),
    Prim {
        prim: KPrim,
        arg_count: usize,
        cont_count: usize,
        result: KSymbol,
        use_result: bool,
        location: Location,
    },
    Jump {
        label: KSymbol,
        arg_count: usize,
    },
    Label {
        label: KSymbol,
        arg_count: usize,
    },
}

struct LoopConstruction {
    break_label: KSymbol,
    continue_label: KSymbol,
}

#[derive(Default)]
struct Xx {
    last_id: usize,
    current: Vec<XCommand>,
    parent_loop: Option<LoopConstruction>,
    extern_fns: Vec<KExternFn>,
    fns: Vec<KFn>,
}

impl Xx {
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
    xx: &mut Xx,
) {
    let result = xx.fresh_symbol(&prim.hint_str(), location.clone());

    gen_term_expr(left, xx);
    gen_term_expr(*right_opt.unwrap(), xx);

    xx.push(XCommand::Prim {
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
    gen_body: impl FnOnce(&mut Xx) -> Flow,
    gen_alt: impl FnOnce(&mut Xx) -> Flow,
    location: Location,
    xx: &mut Xx,
) -> Flow {
    let result = xx.fresh_symbol("cond", location.clone());
    let next_label = xx.fresh_symbol("next", location.clone());

    gen_term_expr(cond, xx);

    xx.push(XCommand::Prim {
        prim: KPrim::If,
        arg_count: 1,
        cont_count: 2,
        result,
        use_result: true,
        location,
    });

    // body:
    let body_flow = gen_body(xx);
    if body_flow == flow::SEQUENTIAL {
        xx.push(XCommand::Jump {
            label: next_label.clone(),
            arg_count: 0,
        });
    }

    // alt:
    let alt_flow = gen_alt(xx);
    if alt_flow == flow::SEQUENTIAL {
        xx.push(XCommand::Jump {
            label: next_label.clone(),
            arg_count: 0,
        });
    }

    // next:
    xx.push(XCommand::Label {
        label: next_label,
        arg_count: 0,
    });

    flow::join(body_flow, alt_flow)
}

fn gen_term_expr(term: PTerm, xx: &mut Xx) {
    match term {
        PTerm::Int(token) => {
            xx.push_term(KTerm::Int(token));
        }
        PTerm::Str(..) => unimplemented!(),
        PTerm::Name(name) => {
            let symbol = xx.name_to_symbol(name);
            xx.push_term(KTerm::Name(symbol));
        }
        PTerm::Call { callee, arg_list } => {
            let location = arg_list.left.into_location();
            let arg_count = arg_list.args.len() + 1;
            let result = xx.fresh_symbol("result", location.clone());

            gen_term_expr(*callee, xx);

            // FIXME: propagate flow?
            for arg in arg_list.args {
                gen_expr(arg.expr, xx);
            }

            xx.push(XCommand::Prim {
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
            BinaryOp::Add => emit_binary_op(KPrim::Add, *left, right_opt, location, xx),
            BinaryOp::Sub => emit_binary_op(KPrim::Sub, *left, right_opt, location, xx),
            BinaryOp::Mul => emit_binary_op(KPrim::Mul, *left, right_opt, location, xx),
            BinaryOp::Div => emit_binary_op(KPrim::Div, *left, right_opt, location, xx),
            BinaryOp::Mod => emit_binary_op(KPrim::Mod, *left, right_opt, location, xx),
            BinaryOp::BitAnd => emit_binary_op(KPrim::BitAnd, *left, right_opt, location, xx),
            BinaryOp::BitOr => emit_binary_op(KPrim::BitOr, *left, right_opt, location, xx),
            BinaryOp::BitXor => emit_binary_op(KPrim::BitXor, *left, right_opt, location, xx),
            BinaryOp::LeftShift => emit_binary_op(KPrim::LeftShift, *left, right_opt, location, xx),
            BinaryOp::RightShift => {
                emit_binary_op(KPrim::RightShift, *left, right_opt, location, xx)
            }
            BinaryOp::Eq => emit_binary_op(KPrim::Eq, *left, right_opt, location, xx),
            BinaryOp::Ne => emit_binary_op(KPrim::Ne, *left, right_opt, location, xx),
            BinaryOp::Lt => emit_binary_op(KPrim::Lt, *left, right_opt, location, xx),
            BinaryOp::Le => emit_binary_op(KPrim::Le, *left, right_opt, location, xx),
            BinaryOp::Gt => emit_binary_op(KPrim::Gt, *left, right_opt, location, xx),
            BinaryOp::Ge => emit_binary_op(KPrim::Ge, *left, right_opt, location, xx),
            BinaryOp::LogAnd => {
                let location1 = location.clone();
                let _ = emit_if(
                    *left,
                    |xx| {
                        gen_term_expr(*right_opt.unwrap(), xx);
                        flow::SEQUENTIAL
                    },
                    move |xx| {
                        xx.push_int(0, location);
                        flow::SEQUENTIAL
                    },
                    location1,
                    xx,
                );
            }
            BinaryOp::LogOr => {
                let location1 = location.clone();
                let _ = emit_if(
                    *left,
                    move |xx| {
                        xx.push_int(1, location);
                        flow::SEQUENTIAL
                    },
                    |xx| {
                        gen_term_expr(*right_opt.unwrap(), xx);
                        flow::SEQUENTIAL
                    },
                    location1,
                    xx,
                );
            }
        },
    }
}

fn gen_expr(expr: PExpr, xx: &mut Xx) -> Flow {
    match expr {
        PExpr::Term { term, .. } => {
            gen_term_expr(term, xx);
            flow::SEQUENTIAL
        }
        PExpr::Block(block) => gen_block(block, xx),
        PExpr::Break { .. } => {
            let dest = xx
                .parent_loop
                .as_ref()
                .expect("out of loop")
                .break_label
                .clone();
            xx.push(XCommand::Jump {
                label: dest,
                arg_count: 0,
            });
            flow::LOOP
        }
        PExpr::Continue { .. } => {
            let dest = xx
                .parent_loop
                .as_ref()
                .expect("out of loop")
                .continue_label
                .clone();
            xx.push(XCommand::Jump {
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
                |xx| gen_block(body_opt.unwrap(), xx),
                |xx| {
                    let mut alt_flow = flow::SEQUENTIAL;
                    if let Some(alt) = alt_opt {
                        alt_flow = gen_expr(*alt, xx);
                    }
                    alt_flow
                },
                location,
                xx,
            )
        }
        PExpr::While {
            keyword,
            cond_opt,
            body_opt,
        } => {
            let location = keyword.into_location();
            let result = xx.fresh_symbol("cond", location.clone());
            let continue_label = xx.fresh_symbol("continue_", location.clone());
            let next_label = xx.fresh_symbol("next", location.clone());

            xx.push(XCommand::Jump {
                label: continue_label.clone(),
                arg_count: 0,
            });

            xx.push(XCommand::Label {
                label: continue_label.clone(),
                arg_count: 0,
            });

            gen_term_expr(cond_opt.unwrap(), xx);

            xx.push(XCommand::Prim {
                prim: KPrim::If,
                arg_count: 1,
                cont_count: 2,
                result,
                use_result: false,
                location,
            });

            let parent_loop = std::mem::replace(
                &mut xx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            let mut flow = gen_block(body_opt.unwrap(), xx);
            if flow == flow::SEQUENTIAL {
                xx.push(XCommand::Jump {
                    label: continue_label.clone(),
                    arg_count: 0,
                });
            }

            // alt:
            xx.push(XCommand::Jump {
                label: next_label.clone(),
                arg_count: 0,
            });

            xx.parent_loop = parent_loop;

            // next:
            xx.push(XCommand::Label {
                label: next_label,
                arg_count: 0,
            });

            xx.push_int(0, Location::new_dummy());

            flow::end_loop(&mut flow);
            flow
        }
        PExpr::Loop { keyword, body_opt } => {
            let location = keyword.into_location();
            let continue_label = xx.fresh_symbol("continue_", location.clone());
            let next_label = xx.fresh_symbol("next", location.clone());

            xx.push(XCommand::Jump {
                label: continue_label.clone(),
                arg_count: 0,
            });

            xx.push(XCommand::Label {
                label: continue_label.clone(),
                arg_count: 0,
            });

            let parent_loop = std::mem::replace(
                &mut xx.parent_loop,
                Some(LoopConstruction {
                    break_label: next_label.clone(),
                    continue_label: continue_label.clone(),
                }),
            );

            // body:
            let mut flow = gen_block(body_opt.unwrap(), xx);
            if flow == flow::SEQUENTIAL {
                xx.push(XCommand::Jump {
                    label: continue_label.clone(),
                    arg_count: 0,
                });
            }

            xx.parent_loop = parent_loop;

            // next:
            xx.push(XCommand::Label {
                label: next_label,
                arg_count: 0,
            });

            xx.push_int(0, Location::new_dummy());

            flow::end_loop(&mut flow);
            flow
        }
    }
}

fn gen_decl(decl: PDecl, xx: &mut Xx) -> Flow {
    match decl {
        PDecl::Expr(expr) => {
            let flow = gen_expr(expr, xx);
            xx.push(XCommand::Pop(1));
            flow
        }
        PDecl::Let {
            keyword,
            name_opt,
            init_opt,
        } => {
            let result = xx.name_to_symbol(name_opt.expect("missing name"));
            let location = keyword.into_location();

            let flow = gen_expr(init_opt.unwrap(), xx);

            xx.push(XCommand::Prim {
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
            let fn_name = xx.fresh_symbol("main", location.clone());
            let return_label = xx.fresh_symbol("return", location.clone());

            let commands = {
                let previous = std::mem::take(&mut xx.current);
                let parent_loop = std::mem::take(&mut xx.parent_loop);

                let arg_count = if block_opt.as_ref().unwrap().last_opt.is_some() {
                    1
                } else {
                    0
                };

                let flow = gen_block(block_opt.unwrap(), xx);
                if flow != flow::FN {
                    xx.push(XCommand::Jump {
                        label: return_label.clone(),
                        arg_count,
                    });
                }

                xx.parent_loop = parent_loop;
                std::mem::replace(&mut xx.current, previous)
            };

            let (node, labels) = {
                let mut gx = Gx::default();
                let node = fold_block(commands, &mut gx);
                (node, gx.labels)
            };

            let k_fn = KFn {
                name: fn_name,
                params: vec![return_label],
                body: node,
                labels,
            };

            xx.fns.push(k_fn);
            flow::SEQUENTIAL
        }
        PDecl::ExternFn {
            name_opt,
            param_list_opt,
            result_opt,
            ..
        } => {
            let fn_name = xx.name_to_symbol(name_opt.unwrap());

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
                    .map(|param| (xx.name_to_symbol(param.name), KTy::I32))
                    .collect(),
                result,
            };

            xx.extern_fns.push(extern_fn);
            flow::SEQUENTIAL
        }
    }
}

fn gen_block(block: PBlock, xx: &mut Xx) -> Flow {
    for decl in block.decls {
        gen_decl(decl, xx)?;
    }

    if let Some(last) = block.last_opt {
        gen_expr(*last, xx)?;
    }

    flow::SEQUENTIAL
}

fn gen_root(root: PRoot, xx: &mut Xx) {
    for decl in root.decls {
        let flow = gen_decl(decl, xx);
        debug_assert_eq!(flow, flow::SEQUENTIAL);
    }
}

#[derive(Default)]
struct Gx {
    stack: Vec<KElement>,
    labels: Vec<KFn>,
}

impl Gx {
    fn push_term(&mut self, term: KTerm) {
        self.stack.push(KElement::Term(term));
    }

    fn push_node(&mut self, node: KNode) {
        self.stack.push(KElement::Node(node));
    }

    fn pop_term(&mut self) -> KTerm {
        match self.stack.pop() {
            Some(KElement::Term(term)) => term,
            top => unreachable!("{:?}", top),
        }
    }

    fn pop_node(&mut self) -> KNode {
        match self.stack.pop() {
            Some(KElement::Node(node)) => node,
            top => unreachable!("{:?}", top),
        }
    }
}

fn do_fold(commands: &mut Vec<XCommand>, gx: &mut Gx) {
    while let Some(command) = commands.pop() {
        match command {
            XCommand::Pop(count) => {
                for _ in 0..count {
                    gx.stack.pop();
                }
            }
            XCommand::Term(term) => {
                gx.push_term(term);
            }
            XCommand::Jump { label, arg_count } => {
                let mut args = vec![];
                for _ in 0..arg_count {
                    args.push(gx.pop_term());
                }
                args.reverse();

                gx.push_node(KNode::Jump { label, args });
                return;
            }
            XCommand::Prim {
                prim,
                arg_count,
                cont_count,
                result,
                use_result,
                location,
            } => {
                let mut args = vec![];
                for _ in 0..arg_count {
                    args.push(gx.pop_term());
                }
                args.reverse();

                let result = result.with_location(location);
                if use_result {
                    gx.push_term(KTerm::Name(result.clone()));
                }

                let mut conts = vec![];
                for _ in 0..cont_count {
                    do_fold(commands, gx);
                    conts.push(gx.pop_node());
                }

                gx.push_node(KNode::Prim {
                    prim,
                    args,
                    results: vec![result],
                    conts,
                });
                return;
            }
            XCommand::Label { label, .. } => {
                do_fold(commands, gx);
                let body = gx.pop_node();

                gx.labels.push(KFn {
                    name: label,
                    params: vec![],
                    body,
                    labels: vec![],
                });
            }
        }
    }
}

fn fold_block(mut commands: Vec<XCommand>, gx: &mut Gx) -> KNode {
    eprintln!("block: {:#?}", commands);
    commands.reverse();

    do_fold(&mut commands, gx);
    let node = gx.pop_node();

    while let Some(XCommand::Label { .. }) = commands.last() {
        do_fold(&mut commands, gx);
    }

    node
}

pub(crate) fn cps_conversion(p_root: PRoot) -> KRoot {
    let mut xx = Xx::default();
    gen_root(p_root, &mut xx);

    KRoot {
        extern_fns: xx.extern_fns,
        fns: xx.fns,
    }
}
