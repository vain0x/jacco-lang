use super::{collect_def_sites, hit_test, Doc, LangService, Location, TPos16};
use crate::lang_service::doc_analysis::DocContentAnalysisMut;

// references と同様
pub(crate) fn definitions(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<Vec<Location>> {
    None
    // let DocContentAnalysisMut {
    //     syntax,
    //     symbols,
    //     cps,
    // } = ls.request_cps(doc)?;

    // let (name, _) = hit_test(doc, pos, syntax, symbols, &ls.mod_outlines, &ls.mods, cps)?;
    // let mut def_sites = vec![];
    // collect_def_sites(doc, name, syntax, symbols, cps, &mut def_sites);

    // Some(def_sites)
}
