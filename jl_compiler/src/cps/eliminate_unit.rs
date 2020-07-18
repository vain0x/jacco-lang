use super::{k_local::KLocalArena, KModData, KModOutline, KNode, KPrim, KTerm, KTyEnv};
use std::{collections::HashSet, mem::swap};

#[derive(Default)]
struct Ex {
    locals: KLocalArena,
    ty_env: KTyEnv,
}

fn on_node(node: &mut KNode, ex: &mut Ex) {
    // unit 型の引数は捨てる。
    match node.prim {
        KPrim::CallDirect | KPrim::Jump => node.args.retain(|arg| match arg {
            KTerm::Unit { .. } => false,
            KTerm::Name(symbol) => !symbol.local.ty(&ex.locals).is_unit(&ex.ty_env),
            _ => true,
        }),
        _ => {}
    }

    for arg in &mut node.args {
        // unit/never 型の変数の参照をリテラルで置き換える。
        // FIXME: never 型の変数は never リテラル (?) に置き換える？
        if let KTerm::Name(symbol) = arg {
            let local_data = &mut ex.locals[symbol.local];
            if local_data.ty.is_unit_or_never(&ex.ty_env) {
                local_data.is_alive = false;
                let location = symbol.location;
                *arg = KTerm::Unit { location };
            }
        }
    }

    for cont in &mut node.conts {
        on_node(cont, ex);
    }
}

pub(crate) fn eliminate_unit(outlines: &mut KModOutline, k_root: &mut KModData) {
    let mut ex = Ex::default();

    let unit_fields = outlines
        .fields
        .enumerate()
        .filter_map(|(k_field, field_data)| {
            if field_data.ty.is_unit() {
                Some(k_field)
            } else {
                None
            }
        })
        .collect::<HashSet<_>>();

    for struct_data in outlines.structs.iter_mut() {
        struct_data
            .fields
            .retain(|k_field| !unit_fields.contains(&k_field));
    }

    // FIXME: zero-sized type の引数はすべて捨てていい。
    // unit 型の引数は捨てる。
    for fn_outline in outlines.fns.iter_mut() {
        fn_outline.param_tys.retain(|ty| !ty.is_unit());
    }

    for fn_data in k_root.fns.iter_mut() {
        swap(&mut ex.ty_env, &mut fn_data.ty_env);
        swap(&mut ex.locals, &mut fn_data.locals);

        // unit 型の引数は捨てる。
        fn_data
            .params
            .retain(|param| !param.ty(&ex.locals).is_unit(&ex.ty_env));

        for label_sig in fn_data.label_sigs.iter_mut() {
            label_sig.param_tys_mut().retain(|ty| !ty.is_unit());
        }

        for local_data in ex.locals.iter_mut() {
            if local_data.ty.is_unit_or_never(&ex.ty_env) {
                local_data.is_alive = false;
            }
        }
        on_node(&mut fn_data.body, &mut ex);

        for label_data in fn_data.labels.iter_mut() {
            // unit 型の引数は捨てる。
            label_data
                .params
                .retain(|param| !param.ty(&ex.locals).is_unit(&ex.ty_env));

            on_node(&mut label_data.body, &mut ex);
        }

        swap(&mut ex.locals, &mut fn_data.locals);
        swap(&mut ex.ty_env, &mut fn_data.ty_env);
    }
}
