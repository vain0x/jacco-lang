use super::{collect_def_sites, collect_use_sites, hit_test, Doc, LangService, Location, TPos16};

pub(crate) fn references(
    doc: Doc,
    pos: TPos16,
    include_definition: bool,
    ls: &mut LangService,
) -> Option<Vec<Location>> {
    let symbols = ls.request_symbols(doc)?;

    let (name, _) = hit_test(doc, pos, symbols)?;
    let mut references = vec![];
    let mut locations = vec![];

    if include_definition {
        collect_def_sites(doc, name, &mut locations, symbols);
        references.extend(
            locations
                .drain(..)
                .map(|location| Location::new(doc, location.range())),
        );
    }

    collect_use_sites(doc, name, &mut locations, symbols);
    references.extend(
        locations
            .drain(..)
            .map(|location| Location::new(doc, location.range())),
    );

    Some(references)
}
