use super::{collect_def_sites, hit_test, Doc, LangService, Location, TPos16};

// references と同様
pub(crate) fn definitions(doc: Doc, pos: TPos16, ls: &mut LangService) -> Option<Vec<Location>> {
    let symbols = ls.request_symbols(doc)?;

    let (name, _) = hit_test(doc, pos, symbols)?;
    let mut def_sites = vec![];
    collect_def_sites(doc, name, &mut def_sites, symbols);

    Some(def_sites)
}
