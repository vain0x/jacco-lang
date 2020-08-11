use super::{
    actions,
    doc_analysis::{AnalysisCache, DocSymbolAnalysisMut, Symbols, Syntax},
};
use crate::{
    cps::{KFn, KLocalVarParent, KModLocalSymbol, KNode, KSymbol, KSymbolCause, KTerm},
    parse::PTree,
    source::{Doc, Loc, TPos16, TRange, TRange16},
};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct Location {
    #[allow(unused)]
    doc: Doc,
    range: TRange,
}

impl Location {
    pub(crate) fn new(doc: Doc, range: TRange) -> Self {
        Self { doc, range }
    }

    pub fn range(&self) -> TRange {
        self.range
    }
}

#[derive(Default)]
pub struct LangService {
    pub(super) docs: HashMap<Doc, AnalysisCache>,
    dirty_sources: HashSet<Doc>,
}

impl LangService {
    pub fn new() -> Self {
        LangService::default()
    }

    pub fn did_initialize(&mut self) {}

    pub fn shutdown(&mut self) {}

    pub(super) fn doc_to_version(&self, doc: Doc) -> Option<i64> {
        self.docs.get(&doc).map(|cache| cache.version())
    }

    #[allow(unused)]
    pub(super) fn request_syntax(&mut self, doc: Doc) -> Option<&mut Syntax> {
        self.docs.get_mut(&doc).map(|cache| cache.request_syntax())
    }

    pub(super) fn request_symbols(&mut self, doc: Doc) -> Option<DocSymbolAnalysisMut<'_>> {
        self.docs.get_mut(&doc).map(|cache| cache.request_symbols())
    }

    pub fn open_doc(&mut self, doc: Doc, version: i64, text: Rc<String>) {
        self.docs.insert(
            doc,
            AnalysisCache {
                doc,
                version,
                text,
                // FIXME: 引数でもらう
                source_path: PathBuf::from("main.jacco").into(),
                syntax_opt: None,
                symbols_opt: None,
            },
        );
        self.dirty_sources.insert(doc);
    }

    pub fn change_doc(&mut self, doc: Doc, version: i64, text: Rc<String>) {
        if let Some(analysis) = self.docs.get_mut(&doc) {
            analysis.set_text(version, text);
            self.dirty_sources.insert(doc);
        }
    }

    pub fn close_doc(&mut self, doc: Doc) {
        self.docs.remove(&doc);
        self.dirty_sources.remove(&doc);
    }

    pub fn completion(&mut self, doc: Doc, pos: TPos16) -> Vec<()> {
        actions::completion(doc, pos, self)
    }

    pub fn definitions(&mut self, doc: Doc, pos: TPos16) -> Option<Vec<Location>> {
        actions::definitions(doc, pos, self)
    }

    pub fn document_highlight(
        &mut self,
        doc: Doc,
        pos: TPos16,
    ) -> Option<(Vec<TRange>, Vec<TRange>)> {
        actions::document_highlight(doc, pos, self)
    }

    pub fn hover(&mut self, doc: Doc, pos: TPos16) -> Option<String> {
        actions::hover(doc, pos, self)
    }

    pub fn references(
        &mut self,
        doc: Doc,
        pos: TPos16,
        include_definition: bool,
    ) -> Option<Vec<Location>> {
        actions::references(doc, pos, include_definition, self)
    }

    pub fn prepare_rename(&mut self, doc: Doc, pos: TPos16) -> Option<()> {
        actions::prepare_rename(doc, pos, self)
    }

    pub fn rename(
        &mut self,
        doc: Doc,
        pos: TPos16,
        new_name: String,
    ) -> Option<Vec<(Location, i64, String)>> {
        actions::rename(doc, pos, new_name, self)
    }

    pub fn validate(&mut self, doc: Doc) -> (Option<i64>, Vec<(TRange, String)>) {
        actions::validate(doc, self)
    }
}

fn to_range16(range: TRange) -> TRange16 {
    TRange16::at(TPos16::from(range.index), TPos16::from(range.len))
}

fn loc_to_range(loc: Loc, tree: &PTree) -> Option<TRange> {
    let opt = (|| {
        let (_, loc) = loc.inner().ok()?;
        let range = loc.range(tree).ok()?;
        Some(range)
    })();

    if opt.is_none() {
        log::trace!("not resolved loc={:?}", loc);
    }

    opt
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum DefOrUse {
    Def,
    Use,
}

type Sites = Vec<(KModLocalSymbol, DefOrUse, TRange)>;

fn collect_symbols(syntax: &Syntax, symbols: &Symbols, sites: &mut Sites) {
    fn go(node: &KNode, k_fn: KFn, syntax: &Syntax, sites: &mut Sites) {
        for arg in &node.args {
            match arg {
                KTerm::Name(KSymbol {
                    local,
                    cause: KSymbolCause::NameUse(_, key),
                }) => {
                    let range = match key.element(&syntax.tree).range(&syntax.tree) {
                        Ok(it) => it,
                        Err(_) => continue,
                    };
                    sites.push((
                        KModLocalSymbol::LocalVar {
                            local_var: *local,
                            parent: KLocalVarParent::Fn(k_fn),
                        },
                        DefOrUse::Use,
                        range,
                    ));
                }
                KTerm::Fn { k_fn, loc } => {
                    let range = match loc_to_range(*loc, &syntax.tree) {
                        Some(it) => it,
                        None => continue,
                    };
                    sites.push((KModLocalSymbol::Fn(*k_fn), DefOrUse::Use, range));
                }
                _ => {}
            }
        }

        for cont in &node.conts {
            go(cont, k_fn, syntax, sites);
        }
    }

    // FIXME: とりあえず関数とローカルのみ。その他のシンボルを追加する
    for ((k_fn, fn_outline), fn_data) in symbols
        .mod_outline
        .fns
        .enumerate()
        .zip(symbols.mod_data.fns.iter())
    {
        for label_data in fn_data.labels.iter() {
            go(&label_data.body, k_fn, syntax, sites);
        }

        for (local_var, local_var_data) in fn_data.locals.enumerate() {
            let range = match loc_to_range(local_var_data.loc, &syntax.tree) {
                Some(it) => it,
                None => continue,
            };
            sites.push((
                KModLocalSymbol::LocalVar {
                    local_var,
                    parent: KLocalVarParent::Fn(k_fn),
                },
                DefOrUse::Use,
                range,
            ));
        }

        let def_site = match loc_to_range(fn_outline.loc, &syntax.tree) {
            Some(it) => it,
            None => continue,
        };

        sites.push((KModLocalSymbol::Fn(k_fn), DefOrUse::Def, def_site));
    }
}

pub(super) fn hit_test(
    doc: Doc,
    pos: TPos16,
    syntax: &Syntax,
    symbols: &Symbols,
) -> Option<(KModLocalSymbol, Location)> {
    let mut sites = vec![];
    collect_symbols(syntax, symbols, &mut sites);

    sites.iter().find_map(|(symbol, _, range)| {
        if to_range16(*range).contains_inclusive(pos) {
            Some((*symbol, Location::new(doc, *range)))
        } else {
            None
        }
    })
}

pub(super) fn collect_def_sites(
    doc: Doc,
    symbol: KModLocalSymbol,
    syntax: &Syntax,
    symbols: &Symbols,
    locations: &mut Vec<Location>,
) {
    let mut sites = vec![];
    collect_symbols(syntax, symbols, &mut sites);

    locations.extend(sites.iter().filter_map(|&(s, def_or_use, range)| {
        if s == symbol && def_or_use == DefOrUse::Def {
            Some(Location::new(doc, range))
        } else {
            None
        }
    }));
}

pub(super) fn collect_use_sites(
    doc: Doc,
    symbol: KModLocalSymbol,
    syntax: &Syntax,
    symbols: &Symbols,
    locations: &mut Vec<Location>,
) {
    let mut sites = vec![];
    collect_symbols(syntax, symbols, &mut sites);

    locations.extend(sites.iter().filter_map(|&(s, def_or_use, range)| {
        if s == symbol && def_or_use == DefOrUse::Use {
            Some(Location::new(doc, range))
        } else {
            None
        }
    }));
}

#[cfg(test)]
mod tests {
    use super::{Doc, LangService};
    use crate::source::{cursor_text::parse_cursor_text, TPos16};

    const DOC: Doc = Doc::new(1);

    fn new_service_from_str(s: &str) -> LangService {
        let mut it = LangService::new();
        it.did_initialize();
        it.open_doc(DOC, 1, s.to_string().into());
        it
    }

    #[test]
    fn test_validate_good() {
        let mut lang_service = new_service_from_str("pub fn main() -> i32 { 0 }");
        let (_, errors) = lang_service.validate(DOC);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_validate_syntax_errors() {
        let mut lang_service = new_service_from_str("fn f() { bad!! syntax!! }");
        let (_, errors) = lang_service.validate(DOC);
        assert_ne!(errors.len(), 0);
    }

    #[test]
    fn test_validate_name_resolution_errors() {
        let mut lang_service = new_service_from_str("fn f() { g(); }");
        let (_, errors) = lang_service.validate(DOC);
        assert_ne!(errors.len(), 0);
    }

    #[test]
    fn test_validate_type_errors() {
        // let mut lang_service = new_service_from_str("fn f() { 2_i32 + 3_f64 }");
        // let (_, errors) = lang_service.validate(DOC);
        // assert_ne!(errors.len(), 0);
    }

    #[test]
    fn test_definition() {
        let mut lang_service = new_service_from_str("fn foo() { foo(); }");

        let defs = lang_service.definitions(DOC, TPos16::from("fn foo"));
        assert_eq!(
            defs.unwrap()
                .into_iter()
                .map(|location| format!("{:?}", location.range()))
                .collect::<Vec<_>>()
                .join("; "),
            "1.4-1.7"
        );

        let defs = lang_service.definitions(DOC, TPos16::from("fn foo() { foo"));
        assert_eq!(
            defs.unwrap()
                .into_iter()
                .map(|location| format!("{:?}", location.range()))
                .collect::<Vec<_>>()
                .join("; "),
            "1.4-1.7"
        );
    }

    #[test]
    fn test_references() {
        let text = r#"
            fn f() {
                let <[foo]> = 2;
                <[foo]> += 4;
                <[foo]> += 8;
            }

            fn g() {
                let foo = "";
            }
        "#;
        let cursor_text = parse_cursor_text(text).unwrap();
        let mut lang_service = new_service_from_str(cursor_text.as_str());

        let cursors = cursor_text.to_pos_vec();
        let mut refs = lang_service.references(DOC, cursors[0].into(), true);
        if let Some(refs) = refs.as_mut() {
            refs.sort();
        }

        assert_eq!(
            refs.into_iter()
                .flatten()
                .map(|location| format!("{:?}", location.range()))
                .collect::<Vec<_>>()
                .join("; "),
            "3.21-3.24; 4.17-4.20; 5.17-5.20"
        );
    }
}
