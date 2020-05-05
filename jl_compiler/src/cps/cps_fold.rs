//! 命令列から CPS ノードを構築する処理

use super::*;

/// Folding context.
#[derive(Default)]
struct Fx {
    labels: Vec<KFn>,
}

fn do_fold(cont_id_opt: Option<XContId>, commands: &mut Vec<XCommand>, fx: &mut Fx) -> KNode {
    let mut node = KNode::Abort;
    let mut ended = None;

    while let Some(command) = commands.pop() {
        match command {
            XCommand::Prim {
                prim,
                args,
                result_opt,
                cont_ids,
                ..
            } => {
                let mut conts = vec![];

                for cont_id in cont_ids {
                    let cont = do_fold(Some(cont_id), commands, fx);
                    conts.push(cont);
                }

                let cont = do_fold(cont_id_opt, commands, fx);
                conts.push(cont);

                node = KNode::Prim {
                    prim,
                    args,
                    results: result_opt.into_iter().collect(),
                    conts,
                };
                break;
            }
            XCommand::Jump {
                label,
                args,
                end_of: cont_id,
            } => {
                node = KNode::Jump { label, args };
                ended = Some(cont_id);
                break;
            }
            XCommand::Label { label, .. } => {
                let body = do_fold(None, commands, fx);

                fx.labels.push(KFn {
                    name: label,
                    params: vec![],
                    body,
                    labels: vec![],
                });
            }
        }
    }

    while cont_id_opt.is_none() || ended != cont_id_opt {
        match commands.pop() {
            None => break,
            Some(XCommand::Prim { .. }) => continue,
            Some(XCommand::Jump {
                end_of: cont_id, ..
            }) => {
                ended = Some(cont_id);
                continue;
            }
            Some(command @ XCommand::Label { .. }) => {
                commands.push(command);
                break;
            }
        }
    }

    node
}

pub(crate) fn fold_block(mut commands: Vec<XCommand>) -> (KNode, Vec<KFn>) {
    let mut fx = Fx::default();

    eprintln!("block: {:#?}", commands);
    commands.reverse();

    let node = do_fold(None, &mut commands, &mut fx);

    while let Some(XCommand::Label { .. }) = commands.last() {
        do_fold(None, &mut commands, &mut fx);
    }

    (node, fx.labels)
}
