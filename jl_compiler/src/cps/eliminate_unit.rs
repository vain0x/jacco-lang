use super::{KLocalData, KNode, KRoot, KTerm, KTyEnv};
use std::mem::{swap, take};

#[derive(Default)]
struct Ex {
    locals: Vec<KLocalData>,
    ty_env: KTyEnv,
}

fn on_node(node: &mut KNode, ex: &mut Ex) {
    for arg in &mut node.args {
        // unit/never 型の変数の参照をリテラルで置き換える。
        // FIXME: never 型の変数は never リテラル (?) に置き換える？
        if let KTerm::Name(symbol) = arg {
            let local_data = &mut ex.locals[symbol.local.id()];
            if ex.ty_env.is_unit_or_never(&local_data.ty) {
                local_data.is_alive = false;
                let location = take(&mut symbol.location);
                *arg = KTerm::Unit { location };
            }
        }
    }

    for cont in &mut node.conts {
        on_node(cont, ex);
    }
}

pub(crate) fn eliminate_unit(k_root: &mut KRoot) {
    let mut ex = Ex::default();

    for fn_data in &mut k_root.fns {
        swap(&mut ex.ty_env, &mut fn_data.ty_env);
        swap(&mut ex.locals, &mut fn_data.locals);

        for local_data in &mut ex.locals {
            if ex.ty_env.is_unit_or_never(&local_data.ty) {
                local_data.is_alive = false;
            }
        }
        on_node(&mut fn_data.body, &mut ex);

        for label_data in &mut fn_data.labels {
            on_node(&mut label_data.body, &mut ex);
        }

        swap(&mut ex.locals, &mut fn_data.locals);
        swap(&mut ex.ty_env, &mut fn_data.ty_env);
    }
}
