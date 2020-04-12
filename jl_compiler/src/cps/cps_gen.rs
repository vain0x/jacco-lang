use super::*;
use crate::parse::*;

#[derive(Debug)]
enum XCommand {
    Term(KTerm),
    Jump { label: String, arity: usize },
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

fn extend_expr(term: PTerm, xx: &mut Xx) {
    match term {
        PTerm::Int(token) => xx.push_term(KTerm::Int(token)),
        PTerm::Name(token) => xx.push_term(KTerm::Name(token)),
        _ => unimplemented!(),
    }
}

fn extend_stmt(stmt: PStmt, xx: &mut Xx) {
    match stmt {
        PStmt::Expr { term, .. } => {
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
                    arity: 1,
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
    fns: Vec<KFn>,
}

fn do_fold(commands: &mut Vec<XCommand>, gx: &mut Gx) -> KElement {
    let default_node = KNode::Abort;

    match commands.pop() {
        None => KElement::Node(default_node),
        Some(XCommand::Term(term)) => KElement::Term(term),
        Some(XCommand::Jump { label, arity }) => {
            let mut args = vec![];
            for _ in 0..arity {
                let arg = match do_fold(commands, gx) {
                    KElement::Term(term) => term,
                    _ => unreachable!(),
                };
                args.push(arg);
            }

            KElement::Node(KNode::Jump { label, args })
        }
    }
}

fn fold_block(mut commands: Vec<XCommand>, gx: &mut Gx) -> KNode {
    let element = do_fold(&mut commands, gx);

    match element {
        KElement::Node(node) => node,
        KElement::Term(..) => unreachable!("{:?}", element),
    }
}

fn fold_root(root: XRoot, gx: &mut Gx) {
    for XFn { name, params, body } in root.fns {
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
