use super::{collect_def_sites, collect_use_sites, hit_test, Doc, LangService, TPos16, TRange};

pub(crate) fn document_highlight(
    doc: Doc,
    pos: TPos16,
    ls: &mut LangService,
) -> Option<(Vec<TRange>, Vec<TRange>)> {
    let symbols = ls.request_symbols(doc)?;

    let (name, _) = hit_test(doc, pos, symbols)?;
    let mut locations = vec![];

    collect_def_sites(doc, name, &mut locations, symbols);
    let def_sites = locations
        .drain(..)
        .map(|location| location.range())
        .collect();

    collect_use_sites(doc, name, &mut locations, symbols);
    let use_sites = locations
        .drain(..)
        .map(|location| location.range())
        .collect();

    Some((def_sites, use_sites))
}
