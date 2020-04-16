use super::*;
use crate::parse::*;
use crate::token::{BinaryOp, Location};

#[derive(Debug)]
enum XCommand {
    Term(KTerm),
    Prim {
        prim: KPrim,
        arg_count: usize,
        location: Location,
    },
    Jump {
        label: String,
        arg_count: usize,
    },
}

struct XBlock(Vec<XCommand>);

struct XFn {
    name: String,
    params: Vec<String>,
    body: XBlock,
}

struct XRoot {
    fns: Vec<XFn>,
}

#[derive(Default)]
struct Xx {
    current: Vec<XCommand>,
    fns: Vec<XFn>,
}

impl Xx {
    fn push(&mut self, command: XCommand) {
        self.current.push(command);
    }

    fn push_term(&mut self, term: KTerm) {
        self.push(XCommand::Term(term));
    }

    fn enter_block(&mut self, extend: impl FnOnce(&mut Xx)) -> Vec<XCommand> {
        let mut other = std::mem::replace(&mut self.current, vec![]);

        extend(self);

        std::mem::swap(&mut self.current, &mut other);
        other
    }
}

fn extend_binary_op(prim: KPrim, left: PTerm, right: PTerm, location: Location, xx: &mut Xx) {
    extend_expr(left, xx);
    extend_expr(right, xx);
    xx.push(XCommand::Prim {
        prim,
        arg_count: 2,
        location,
    });
}

fn extend_expr(term: PTerm, xx: &mut Xx) {
    match term {
        PTerm::Int(token) => xx.push_term(KTerm::Int(token)),
        PTerm::Name(token) => {
            let (_, text, location) = token.decompose();
            xx.push_term(KTerm::Name { text, location });
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
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn extend_stmt(stmt: PStmt, xx: &mut Xx) {
    match stmt {
        PStmt::Expr { .. } => {
            //
        }
        PStmt::Fn { block_opt, .. } => {
            let ret = "ret".to_string();

            let commands = xx.enter_block(|xx| {
                let block = block_opt.unwrap();
                let last = block.last_opt.unwrap();

                extend_expr(last, xx);
                xx.push(XCommand::Jump {
                    label: ret.to_string(),
                    arg_count: 1,
                });
            });

            let x_fn = XFn {
                name: "main".to_string(),
                params: vec![ret],
                body: XBlock(commands),
            };

            xx.fns.push(x_fn);
        }
    }
}

fn extend_root(root: PRoot, xx: &mut Xx) {
    for body in root.body {
        extend_stmt(body, xx);
    }
}

#[derive(Default)]
struct Gx {
    last_id: usize,
    stack: Vec<KElement>,
    fns: Vec<KFn>,
}

impl Gx {
    fn fresh_name(&mut self, hint: &str) -> String {
        self.last_id += 1;
        let id = self.last_id;
        format!("{}_{}", hint, id)
    }

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
                location,
            } => {
                let mut args = vec![];
                for _ in 0..arg_count {
                    args.push(gx.pop_term());
                }
                args.reverse();

                let result = gx.fresh_name(&prim.hint_str());
                gx.push_term(KTerm::Name {
                    text: result.to_string(),
                    location,
                });

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

fn fold_root(root: XRoot, gx: &mut Gx) {
    for XFn { name, params, body } in root.fns {
        eprintln!("{} -> {:#?}", name, body.0);

        let body = fold_block(body.0, gx);
        gx.fns.push(KFn { name, params, body });
    }
}

pub(crate) fn cps_conversion(p_root: PRoot) -> KRoot {
    let x_root = {
        let mut xx = Xx::default();
        extend_root(p_root, &mut xx);
        XRoot { fns: xx.fns }
    };

    let k_root = {
        let mut gx = Gx::default();
        fold_root(x_root, &mut gx);
        KRoot { fns: gx.fns }
    };

    k_root
}
