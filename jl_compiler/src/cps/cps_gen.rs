use self::flow::Flow;
use super::*;
use crate::parse::*;

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
}

fn extend_binary_op(prim: KPrim, left: PTerm, right: PTerm, location: Location, xx: &mut Xx) {
    let result = xx.fresh_symbol(&prim.hint_str(), location.clone());

    extend_expr(left, xx);
    extend_expr(right, xx);
    xx.push(XCommand::Prim {
        prim,
        arg_count: 2,
        cont_count: 1,
        result,
        use_result: true,
        location,
    });
}

fn extend_expr(term: PTerm, xx: &mut Xx) {
    match term {
        PTerm::Int(token) => {
            xx.push_term(KTerm::Int(token));
        }
        PTerm::Name(name) => {
            let symbol = xx.name_to_symbol(name);
            xx.push_term(KTerm::Name(symbol));
        }
        PTerm::BinaryOp {
            op,
            left,
            right,
            location,
        } => match op {
            BinaryOp::Add => extend_binary_op(KPrim::Add, *left, *right, location, xx),
            BinaryOp::Sub => extend_binary_op(KPrim::Sub, *left, *right, location, xx),
            BinaryOp::Mul => extend_binary_op(KPrim::Mul, *left, *right, location, xx),
            BinaryOp::Div => extend_binary_op(KPrim::Div, *left, *right, location, xx),
            BinaryOp::Mod => extend_binary_op(KPrim::Mod, *left, *right, location, xx),
            BinaryOp::BitAnd => extend_binary_op(KPrim::BitAnd, *left, *right, location, xx),
            BinaryOp::BitOr => extend_binary_op(KPrim::BitOr, *left, *right, location, xx),
            BinaryOp::BitXor => extend_binary_op(KPrim::BitXor, *left, *right, location, xx),
            BinaryOp::LeftShift => extend_binary_op(KPrim::LeftShift, *left, *right, location, xx),
            BinaryOp::RightShift => {
                extend_binary_op(KPrim::RightShift, *left, *right, location, xx)
            }
            BinaryOp::Eq => extend_binary_op(KPrim::Eq, *left, *right, location, xx),
            BinaryOp::Ne => extend_binary_op(KPrim::Ne, *left, *right, location, xx),
            BinaryOp::Lt => extend_binary_op(KPrim::Lt, *left, *right, location, xx),
            BinaryOp::Le => extend_binary_op(KPrim::Le, *left, *right, location, xx),
            BinaryOp::Gt => extend_binary_op(KPrim::Gt, *left, *right, location, xx),
            BinaryOp::Ge => extend_binary_op(KPrim::Ge, *left, *right, location, xx),
        },
        _ => unimplemented!(),
    }
}

fn extend_fn_stmt(block_opt: Option<PBlock>, location: Location, xx: &mut Xx) {
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

        let flow = extend_block(block_opt.unwrap(), xx);
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
}

fn extend_extern_fn_stmt(
    name_opt: Option<PName>,
    param_list: PParamList,
    result_opt: Option<PResult>,
    xx: &mut Xx,
) {
    let fn_name = match name_opt {
        Some(name) => xx.name_to_symbol(name),
        None => return,
    };

    let result = match result_opt {
        Some(result) => match result.ty_opt.unwrap().text.as_str() {
            "i32" => KTy::I32,
            _ => unimplemented!(),
        },
        None => KTy::Unit,
    };

    let extern_fn = KExternFn {
        name: fn_name,
        params: param_list
            .params
            .into_iter()
            .map(|param| (xx.name_to_symbol(param.name), KTy::I32))
            .collect(),
        result,
    };

    xx.extern_fns.push(extern_fn);
}

fn extend_block(block: PBlock, xx: &mut Xx) -> Flow {
    for stmt in block.body {
        extend_stmt(stmt, xx)?;
    }

    if let Some(last) = block.last_opt {
        extend_expr(last, xx);
    }
    flow::SEQUENTIAL
}

fn extend_stmt(stmt: PStmt, xx: &mut Xx) -> Flow {
    match stmt {
        PStmt::Expr { .. } => flow::SEQUENTIAL,
        PStmt::Block(block) => extend_block(block, xx),
        PStmt::Break { .. } => {
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
        PStmt::Continue { .. } => {
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
        PStmt::If {
            keyword,
            cond_opt,
            body_opt,
            alt_opt,
            ..
        } => {
            let location = keyword.into_location();
            let result = xx.fresh_symbol("cond", location.clone());
            let next_label = xx.fresh_symbol("next", location.clone());

            extend_expr(cond_opt.unwrap(), xx);

            xx.push(XCommand::Prim {
                prim: KPrim::If,
                arg_count: 1,
                cont_count: 2,
                result,
                use_result: true,
                location,
            });

            // body:
            let body_flow = extend_block(body_opt.unwrap(), xx);
            if body_flow == flow::SEQUENTIAL {
                xx.push(XCommand::Jump {
                    label: next_label.clone(),
                    arg_count: 0,
                });
            }

            // alt:
            let mut alt_flow = flow::SEQUENTIAL;
            if let Some(alt) = alt_opt {
                alt_flow = extend_stmt(*alt, xx);
            }

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
        PStmt::While {
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

            extend_expr(cond_opt.unwrap(), xx);

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
            let mut flow = extend_block(body_opt.unwrap(), xx);
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

            flow::end_loop(&mut flow);
            flow
        }
        PStmt::Loop { keyword, body_opt } => {
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
            let mut flow = extend_block(body_opt.unwrap(), xx);
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

            flow::end_loop(&mut flow);
            flow
        }
        PStmt::Let {
            keyword,
            name_opt,
            init_opt,
        } => {
            let result = xx.name_to_symbol(name_opt.expect("missing name"));
            let location = keyword.into_location();

            extend_expr(init_opt.unwrap(), xx);

            xx.push(XCommand::Prim {
                prim: KPrim::Let,
                arg_count: 1,
                cont_count: 1,
                result,
                use_result: false,
                location,
            });

            flow::SEQUENTIAL
        }
        PStmt::Fn { keyword, block_opt } => {
            extend_fn_stmt(block_opt, keyword.into_location(), xx);
            flow::SEQUENTIAL
        }
        PStmt::ExternFn {
            name_opt,
            param_list_opt,
            result_opt,
            ..
        } => {
            extend_extern_fn_stmt(name_opt, param_list_opt.unwrap(), result_opt, xx);
            flow::SEQUENTIAL
        }
    }
}

fn extend_root(root: PRoot, xx: &mut Xx) {
    for stmt in root.body {
        let flow = extend_stmt(stmt, xx);
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
    extend_root(p_root, &mut xx);

    KRoot {
        extern_fns: xx.extern_fns,
        fns: xx.fns,
    }
}
