use super::*;
use crate::parse::*;

#[derive(Debug)]
enum XCommand {
    Term(KTerm),
    Prim {
        prim: KPrim,
        arg_count: usize,
        result: KSymbol,
        location: Location,
    },
    Jump {
        label: KSymbol,
        arg_count: usize,
    },
}

#[derive(Default)]
struct Xx {
    last_id: usize,
    current: Vec<XCommand>,
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

    fn fresh_symbol(&mut self, hint: &str) -> KSymbol {
        self.last_id += 1;
        let id = self.last_id;

        let text = hint.to_string();

        // FIXME: ヒントを与える。
        let location = Location::new_dummy();

        KSymbol { id, text, location }
    }

    fn push(&mut self, command: XCommand) {
        self.current.push(command);
    }

    fn push_term(&mut self, term: KTerm) {
        self.push(XCommand::Term(term));
    }

    fn enter_block(&mut self, extend: impl FnOnce(&mut Xx)) -> KNode {
        let commands = {
            let previous = std::mem::take(&mut self.current);
            extend(self);
            std::mem::replace(&mut self.current, previous)
        };

        let node = {
            let mut gx = Gx::default();
            fold_block(commands, &mut gx)
        };

        node
    }
}

fn extend_binary_op(prim: KPrim, left: PTerm, right: PTerm, location: Location, xx: &mut Xx) {
    let result = xx.fresh_symbol(&prim.hint_str());

    extend_expr(left, xx);
    extend_expr(right, xx);
    xx.push(XCommand::Prim {
        prim,
        arg_count: 2,
        result,
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

fn extend_fn_stmt(block_opt: Option<PBlock>, xx: &mut Xx) {
    let fn_name = xx.fresh_symbol("main");
    let return_label = xx.fresh_symbol("return");

    let body = xx.enter_block(|xx| {
        let block = block_opt.unwrap();

        for stmt in block.body {
            extend_stmt(stmt, xx);
        }

        let last = block.last_opt.unwrap();

        extend_expr(last, xx);
        xx.push(XCommand::Jump {
            label: return_label.clone(),
            arg_count: 1,
        });
    });

    let k_fn = KFn {
        name: fn_name,
        params: vec![return_label],
        body,
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

fn extend_stmt(stmt: PStmt, xx: &mut Xx) {
    match stmt {
        PStmt::Expr { .. } => {
            //
        }
        PStmt::If {
            keyword, cond_opt, ..
        } => {
            let result = xx.fresh_symbol("cond");

            extend_expr(cond_opt.unwrap(), xx);

            xx.push(XCommand::Prim {
                prim: KPrim::If,
                arg_count: 1,
                result,
                location: keyword.into_location(),
            });

            // FIXME: generate body
            // let body = body_opt.unwrap();
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
                result,
                location,
            });
        }
        PStmt::Fn { block_opt, .. } => extend_fn_stmt(block_opt, xx),
        PStmt::ExternFn {
            name_opt,
            param_list_opt,
            result_opt,
            ..
        } => extend_extern_fn_stmt(name_opt, param_list_opt.unwrap(), result_opt, xx),
    }
}

fn extend_decl(stmt: PStmt, xx: &mut Xx) {
    match stmt {
        PStmt::Fn { block_opt, .. } => extend_fn_stmt(block_opt, xx),
        PStmt::ExternFn {
            name_opt,
            param_list_opt,
            result_opt,
            ..
        } => extend_extern_fn_stmt(name_opt, param_list_opt.unwrap(), result_opt, xx),
        _ => unimplemented!(),
    }
}

fn extend_root(root: PRoot, xx: &mut Xx) {
    for body in root.body {
        extend_decl(body, xx);
    }
}

#[derive(Default)]
struct Gx {
    stack: Vec<KElement>,
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
            _ => unreachable!(),
        }
    }

    fn pop_node(&mut self) -> KNode {
        match self.stack.pop() {
            Some(KElement::Node(node)) => node,
            _ => unreachable!(),
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
                result,
                location,
            } => {
                let mut args = vec![];
                for _ in 0..arg_count {
                    args.push(gx.pop_term());
                }
                args.reverse();

                let result = result.with_location(location);
                gx.push_term(KTerm::Name(result.clone()));

                do_fold(commands, gx);
                let cont = gx.pop_node();

                gx.push_node(KNode::Prim {
                    prim,
                    args,
                    results: vec![result],
                    conts: vec![cont],
                });
                return;
            }
        }
    }
}

fn fold_block(mut commands: Vec<XCommand>, gx: &mut Gx) -> KNode {
    commands.reverse();

    do_fold(&mut commands, gx);
    gx.pop_node()
}

pub(crate) fn cps_conversion(p_root: PRoot) -> KRoot {
    let mut xx = Xx::default();
    extend_root(p_root, &mut xx);

    KRoot {
        extern_fns: xx.extern_fns,
        fns: xx.fns,
    }
}
