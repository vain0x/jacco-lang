use super::{Doc, LangService, TPos16};

pub(crate) fn hover(_doc: Doc, _pos: TPos16, _ls: &mut LangService) -> Option<String> {
    // let (name, _) = {
    //     let symbols = ls.request_symbols(doc)?;
    //     hit_test(doc, pos, symbols)?
    // };

    // let cps = ls.request_cps(doc)?;
    // let ty = name.ty(&cps.root);
    // let ty_env = name.ty_env(&cps.root);
    // let mod_outlines = todo!();
    // Some(ty.display(ty_env, &mod_outlines))
    None
}
