//! 命令列から CPS ノードを構築する処理

use super::*;
use std::iter::repeat_with;

/// Folding context.
#[derive(Default)]
struct Fx {
    labels: Vec<KFn>,
}

fn do_fold(commands: &mut Vec<KCommand>, fx: &mut Fx) -> KNode {
    while let Some(command) = commands.pop() {
        match command {
            KCommand::Node {
                prim,
                tys,
                args,
                result_opt,
                cont_count,
                ..
            } => {
                let conts = repeat_with(|| do_fold(commands, fx))
                    .take(cont_count)
                    .collect();

                return KNode {
                    prim,
                    tys,
                    args,
                    results: result_opt.into_iter().collect(),
                    conts,
                };
            }
            KCommand::Label { label, params } => {
                let body = do_fold(commands, fx);

                fx.labels.push(KFn {
                    name: label,
                    params: params.into_iter().collect(),
                    body,
                    labels: vec![],
                });
            }
        }
    }

    KNode::default()
}

pub(crate) fn fold_block(mut commands: Vec<KCommand>) -> (KNode, Vec<KFn>) {
    let mut fx = Fx::default();

    trace!("block: {:#?}", commands);
    commands.reverse();

    let node = do_fold(&mut commands, &mut fx);

    while let Some(KCommand::Label { .. }) = commands.last() {
        do_fold(&mut commands, &mut fx);
    }

    (node, fx.labels)
}
