use super::{Doc, LangService, TPos16};
use crate::{
    cps::{KLocalVarParent, KModLocalSymbol, KTyEnv},
    lang_service::{
        doc_analysis::DocSymbolAnalysisMut,
        lang_service::{hit_test, Content},
    },
};

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
        KModLocalSymbol::Fn(_) => {}
        KModLocalSymbol::ExternFn(_) => {}
        KModLocalSymbol::Enum(_) => {}
        KModLocalSymbol::Struct(_) => {}
        KModLocalSymbol::Alias(_) => {}
    }

    contents_opt
}
