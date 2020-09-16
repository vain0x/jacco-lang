use super::{collect_def_sites, collect_use_sites, hit_test, Doc, LangService, Location, TPos16};
use crate::lang_service::doc_analysis::DocContentAnalysisMut;

pub(crate) fn prepare_rename(_doc: Doc, _pos: TPos16, _ls: &mut LangService) -> Option<()> {
    None
}

pub(crate) fn rename(
    doc: Doc,
    pos: TPos16,
    new_name: String,
    ls: &mut LangService,
) -> Option<Vec<(Location, i64, String)>> {
    //     let version = ls.doc_to_version(doc)?;
    //     let DocContentAnalysisMut {
    //         syntax,
    //         symbols,
    //         cps,
    //     } = ls.request_cps(doc)?;

    //     let (name, _) = hit_test(doc, pos, syntax, symbols, cps)?;

    //     let mut locations = vec![];
    //     collect_def_sites(doc, name, syntax, symbols, cps, &mut locations);
    //     collect_use_sites(doc, name, syntax, symbols, cps, &mut locations);

    //     let edits = locations
    //         .into_iter()
    //         .map(|location| (location, version, new_name.to_string()))
    //         .collect();
    //     Some(edits)
    None
}
