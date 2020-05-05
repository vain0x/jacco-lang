//! 命令列から CPS ノードを構築する処理

use super::*;

/// Folding context.
#[derive(Default)]
struct Fx {
    labels: Vec<KFn>,
}

fn do_fold(commands: &mut Vec<XCommand>, fx: &mut Fx) -> KNode {
    while let Some(command) = commands.pop() {
        match command {
            XCommand::Prim {
                prim,
                args,
                result_opt,
                cont_count,
                ..
            } => {
                let conts = std::iter::repeat_with(|| do_fold(commands, fx))
                    .take(cont_count)
                    .collect();

                return KNode {
                    prim,
                    args,
                    results: result_opt.into_iter().collect(),
                    conts,
                };
            }
            XCommand::Label { label, .. } => {
                let body = do_fold(commands, fx);

                fx.labels.push(KFn {
                    name: label,
                    params: vec![],
                    body,
                    labels: vec![],
                });
            }
        }
    }

    KNode::default()
}

pub(crate) fn fold_block(mut commands: Vec<XCommand>) -> (KNode, Vec<KFn>) {
    let mut fx = Fx::default();

    eprintln!("block: {:#?}", commands);
    commands.reverse();

    let node = do_fold(&mut commands, &mut fx);

    while let Some(XCommand::Label { .. }) = commands.last() {
        do_fold(&mut commands, &mut fx);
    }

    (node, fx.labels)
}
