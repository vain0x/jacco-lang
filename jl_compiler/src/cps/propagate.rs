use std::mem::{replace, take};

use super::*;
use crate::utils::*;

#[derive(Default)]
struct VarInfo {
    is_addressable: bool,
    is_mutable: bool,
    init_opt: Option<VarInit>,
}

#[derive(Clone)]
enum VarInit {
    Let(KTerm),
}

struct Px<'a> {
    #[allow(unused)]
    mod_outline: &'a KModOutline,
    fn_data: &'a mut KFnData,
    label_done: VecArena<KLabelTag, bool>,
    var_info_arena: VecArena<KLocalVarTag, VarInfo>,
}

impl<'a> Px<'a> {
    fn new(mod_outline: &'a KModOutline, fn_data: &'a mut KFnData) -> Self {
        let label_done = fn_data.labels.slice().map_with_value(false);
        let var_info_arena = fn_data.local_vars.slice().map_with(Default::default);

        Self {
            mod_outline,
            fn_data,
            label_done,
            var_info_arena,
        }
    }

    fn on_term_use(&self, term: &mut KTerm) {
        match term {
            KTerm::Name(var_term) => {
                let info = var_term.local_var.of(&self.var_info_arena);
                if info.is_addressable || info.is_mutable {
                    return;
                }

                match &info.init_opt {
                    Some(VarInit::Let(other_term)) => {
                        *term = other_term.clone();
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    fn on_let_node(&mut self, node: &mut KNode) -> bool {
        match (
            node.args.as_slice(),
            node.results.as_slice(),
            node.conts.as_mut_slice(),
        ) {
            ([arg], [result], [cont]) => {
                let info = result.local_var.of_mut(&mut self.var_info_arena);
                if info.is_addressable || info.is_mutable {
                    return false;
                }

                let old = info.init_opt.replace(VarInit::Let(arg.clone()));
                assert!(old.is_none());

                *node = take(cont);
                self.on_node(node);
                true
            }
            _ => unreachable!(),
        }
    }

    fn on_node(&mut self, node: &mut KNode) {
        for term in node.args.iter_mut() {
            self.on_term_use(term);
        }

        let is_done = match node.prim {
            KPrim::Let => self.on_let_node(node),
            _ => false,
        };
        if is_done {
            return;
        }

        for cont in node.conts.iter_mut() {
            self.on_node(cont);
        }
    }

    fn on_label(&mut self, label: KLabel) {
        if let Some(parent) = label.of(&self.fn_data.labels).parent_opt {
            self.on_label(parent);
        }

        let is_done = replace(label.of_mut(&mut self.label_done), true);
        if is_done {
            return;
        }

        let mut body = take(&mut label.of_mut(&mut self.fn_data.labels).body);
        self.on_node(&mut body);
        label.of_mut(&mut self.fn_data.labels).body = body;
    }

    fn execute(&mut self) {
        let labels = self.fn_data.labels.keys().collect::<Vec<_>>();

        // 情報収集フェイズ:
        let mut var_info_arena = take(&mut self.var_info_arena);
        let mut stack = self
            .fn_data
            .labels
            .iter()
            .map(|label_data| &label_data.body)
            .collect::<Vec<_>>();

        while let Some(node) = stack.pop() {
            stack.extend(node.conts.iter().rev());

            match node.prim {
                KPrim::Ref => {
                    for term in &node.args {
                        if let KTerm::Name(var_term) = term {
                            let info = var_term.local_var.of_mut(&mut var_info_arena);
                            info.is_addressable = true;
                        }
                    }
                }
                KPrim::RefMut => {
                    for term in &node.args {
                        if let KTerm::Name(var_term) = term {
                            let info = var_term.local_var.of_mut(&mut var_info_arena);
                            info.is_mutable = true;
                        }
                    }
                }
                _ => {}
            }
        }
        self.var_info_arena = var_info_arena;

        // 適用フェイズ:
        for label in labels {
            self.on_label(label);
        }

        // let で定義されて、左辺値として利用されなかった変数はいらない。
        let var_info_arena = take(&mut self.var_info_arena);

        for (local_var, info) in self
            .fn_data
            .local_vars
            .iter_mut()
            .zip(var_info_arena.iter())
        {
            match info.init_opt {
                Some(VarInit::Let(..)) => {
                    if !info.is_addressable && !info.is_mutable {
                        local_var.is_alive = false;
                    }
                }
                _ => {}
            }
        }
    }
}

pub(crate) fn propagate<'a>(mod_outline: &'a KModOutline, mod_data: &'a mut KModData) {
    for fn_data in mod_data.fns.iter_mut() {
        Px::new(mod_outline, fn_data).execute();
    }
}
