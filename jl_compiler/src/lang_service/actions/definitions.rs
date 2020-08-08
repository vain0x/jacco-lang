use super::{collect_def_sites, hit_test, Doc, LangService, Location, TPos16};
use crate::lang_service::doc_analysis::DocSymbolAnalysisMut;

// references と同様
pub(crate) fn definitions(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<Vec<Location>> {
    let DocSymbolAnalysisMut { syntax, symbols } = ls.request_symbols(doc)?;

    let (name, _) = hit_test(doc, pos, syntax, symbols)?;
    let mut def_sites = vec![];
    collect_def_sites(doc, name, syntax, symbols, &mut def_sites);

    Some(def_sites)
}
