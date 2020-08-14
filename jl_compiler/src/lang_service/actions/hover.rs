use super::{Doc, LangService, TPos16};
use crate::{
    cps::{KLocalVarParent, KModLocalSymbol, KTyEnv},
    lang_service::{
        doc_analysis::DocSymbolAnalysisMut,
        lang_service::{hit_test, Content},
    },
};
use std::io::Write;

pub(crate) fn hover(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<Content> {
    ls.request_types();

    let DocSymbolAnalysisMut { syntax, symbols } = ls.request_symbols(doc)?;
    let (symbol, _) = hit_test(doc, pos, syntax, symbols)?;
    let mut contents_opt = None;

    match symbol {
        KModLocalSymbol::LocalVar { parent, local_var } => {
            ls.do_with_mods(|ls, mod_outlines, mods| {
                let k_mod = ls.docs.get(&doc).unwrap().mod_opt.unwrap();
                let mod_data = k_mod.of_mut(mods);

                let ty_env = match parent {
                    KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).ty_env,
                    KLocalVarParent::ExternFn(_) => KTyEnv::EMPTY,
                };

                let locals = match parent {
                    KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).locals,
                    KLocalVarParent::ExternFn(extern_fn) => {
                        &extern_fn.of(&mod_data.extern_fns).locals
                    }
                };

                contents_opt = Some(Content::JaccoCode(
                    local_var.ty(locals).display(ty_env, mod_outlines),
                ));
            });
        }
        KModLocalSymbol::Const(_) => {}
        KModLocalSymbol::StaticVar(_) => {}
        KModLocalSymbol::Fn(k_fn) => {
            let mut out = Vec::new();

            ls.do_with_mods(|ls, mod_outlines, mods| {
                let k_mod = ls.docs.get(&doc).unwrap().mod_opt.unwrap();
                let mod_outline = k_mod.of(mod_outlines);
                let mod_data = k_mod.of(mods);
                let fn_outline = k_fn.of(&mod_outline.fns);
                let fn_data = k_fn.of(&mod_data.fns);

                write!(out, "fn {}(", &fn_outline.name).unwrap();

                for (i, symbol) in fn_data.params.iter().enumerate() {
                    if i != 0 {
                        write!(out, ", ").unwrap();
                    }

                    let local_var = symbol.local.of(&fn_data.locals);
                    write!(
                        out,
                        "{}: {}",
                        &local_var.name,
                        local_var.ty.display(KTyEnv::EMPTY, mod_outlines)
                    )
                    .unwrap();
                }
                write!(out, ")").unwrap();

                if !fn_outline.result_ty.is_unit() {
                    let ty = fn_outline
                        .result_ty
                        .to_ty2(k_mod)
                        .display(KTyEnv::EMPTY, mod_outlines);
                    write!(out, " -> {}", ty).unwrap();
                }

                write!(out, ";").unwrap();
                let text = unsafe { String::from_utf8_unchecked(out) };

                contents_opt = Some(Content::JaccoCode(text));
            });
        }
        KModLocalSymbol::ExternFn(_) => {}
        KModLocalSymbol::Enum(_) => {}
        KModLocalSymbol::Struct(_) => {}
        KModLocalSymbol::Alias(_) => {}
    }

    contents_opt
}
