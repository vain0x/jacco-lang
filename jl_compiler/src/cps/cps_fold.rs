use super::*;

#[derive(Default)]
struct Fx {
    stack: Vec<KElement>,
    labels: Vec<KFn>,
}

impl Fx {
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

fn do_fold(commands: &mut Vec<XCommand>, fx: &mut Fx) {
    while let Some(command) = commands.pop() {
        match command {
            XCommand::Pop(count) => {
                for _ in 0..count {
                    fx.stack.pop();
                }
            }
            XCommand::Term(term) => {
                fx.push_term(term);
            }
            XCommand::Jump { label, arg_count } => {
                let mut args = vec![];
                for _ in 0..arg_count {
                    args.push(fx.pop_term());
                }
                args.reverse();

                fx.push_node(KNode::Jump { label, args });
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
                    args.push(fx.pop_term());
                }
                args.reverse();

                let result = result.with_location(location);
                if use_result {
                    fx.push_term(KTerm::Name(result.clone()));
                }

                let mut conts = vec![];
                for _ in 0..cont_count {
                    do_fold(commands, fx);
                    conts.push(fx.pop_node());
                }

                fx.push_node(KNode::Prim {
                    prim,
                    args,
                    results: vec![result],
                    conts,
                });
                return;
            }
            XCommand::Label { label, .. } => {
                do_fold(commands, fx);
                let body = fx.pop_node();

                fx.labels.push(KFn {
                    name: label,
                    params: vec![],
                    body,
                    labels: vec![],
                });
            }
        }
    }
}

pub(crate) fn fold_block(mut commands: Vec<XCommand>) -> (KNode, Vec<KFn>) {
    let mut fx = Fx::default();

    eprintln!("block: {:#?}", commands);
    commands.reverse();

    do_fold(&mut commands, &mut fx);
    let node = fx.pop_node();

    while let Some(XCommand::Label { .. }) = commands.last() {
        do_fold(&mut commands, &mut fx);
    }

    (node, fx.labels)
}
