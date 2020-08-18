use super::{collect_def_sites, collect_use_sites, hit_test, Doc, LangService, Location, TPos16};
use crate::lang_service::doc_analysis::DocContentAnalysisMut;

pub(crate) fn references(
    doc: Doc,
    pos: TPos16,
    include_definition: bool,
    ls: &mut LangService,
) -> Option<Vec<Location>> {
    let DocContentAnalysisMut {
        syntax,
        symbols,
        cps,
    } = ls.request_cps(doc)?;

    let (name, _) = hit_test(doc, pos, syntax, symbols, cps)?;
    let mut ref_sites = vec![];

    if include_definition {
        collect_def_sites(doc, name, syntax, symbols, cps, &mut ref_sites);
    }
    collect_use_sites(doc, name, syntax, symbols, cps, &mut ref_sites);

    Some(ref_sites)
}
