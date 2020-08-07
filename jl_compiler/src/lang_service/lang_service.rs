use super::{
    actions,
    doc_analysis::{AnalysisCache, Cps, Symbols, Syntax},
};
use crate::{
    cps::{KModData, KTy2, KTyEnv},
    front::{NAbsName, NName, NParentFn},
    source::{Doc, TPos16, TRange},
};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

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
    fn request_syntax(&mut self, doc: Doc) -> Option<&mut Syntax> {
        self.docs.get_mut(&doc).map(|cache| cache.request_syntax())
    }

    pub(super) fn request_symbols(&mut self, doc: Doc) -> Option<&mut Symbols> {
        self.docs.get_mut(&doc).map(|cache| cache.request_symbols())
    }

    #[allow(unused)]
    fn request_cps(&mut self, _doc: Doc) -> Option<&mut Cps> {
        // 頻繁にクラッシュするので無効化
        // self.docs.get_mut(&doc).map(|cache| cache.request_cps())
        None
    }

    #[allow(unused)]
    fn hit_test(&mut self, doc: Doc, pos: TPos16) -> Option<(NAbsName, Location)> {
        let symbols = self.request_symbols(doc)?;
        hit_test(doc, pos, symbols)
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
                cps_opt: None,
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

impl NParentFn {
    fn ty_env(self, k_root: &KModData) -> &KTyEnv {
        match self {
            NParentFn::Fn(k_fn) => &k_fn.of(&k_root.fns).ty_env,
            NParentFn::ExternFn(_) => KTyEnv::EMPTY,
        }
    }
}

impl NAbsName {
    #[allow(unused)]
    pub(crate) fn ty(self, k_root: &KModData) -> &KTy2 {
        match self {
            NAbsName::Unresolved => return &KTy2::Unresolved,
            NAbsName::LocalVar {
                parent_fn: NParentFn::Fn(k_fn),
                local,
            } => return &local.of(&k_fn.of(&k_root.fns).locals).ty,
            NAbsName::LocalVar {
                parent_fn: NParentFn::ExternFn(extern_fn),
                local,
            } => return &local.of(&extern_fn.of(&k_root.extern_fns).locals).ty,
            NAbsName::Other(_) => {
                // FIXME: 実装
                &KTy2::Unresolved
            }
        }
    }

    #[allow(unused)]
    pub(crate) fn ty_env(self, k_root: &KModData) -> &KTyEnv {
        match self {
            NAbsName::LocalVar { parent_fn, .. } => parent_fn.ty_env(k_root),
            NAbsName::Other(NName::Fn(k_fn)) => &k_fn.of(&k_root.fns).ty_env,
            _ => KTyEnv::EMPTY,
        }
    }
}

pub(super) fn hit_test(doc: Doc, pos: TPos16, symbols: &Symbols) -> Option<(NAbsName, Location)> {
    symbols
        .occurrences
        .def_sites
        .iter()
        .chain(symbols.occurrences.use_sites.iter())
        .find_map(|(&name, locations)| {
            locations.iter().find_map(|&location| {
                if location.range().contains_loosely_pos16(pos) {
                    Some((
                        name,
                        Location {
                            doc,
                            range: location.range().into(),
                        },
                    ))
                } else {
                    None
                }
            })
        })
}

pub(super) fn collect_def_sites(
    doc: Doc,
    name: NAbsName,
    locations: &mut Vec<Location>,
    symbols: &mut Symbols,
) {
    locations.extend(
        symbols
            .occurrences
            .def_sites
            .get(&name)
            .iter()
            .flat_map(|locations| locations.iter().map(|location| location.range()))
            .map(|range| Location::new(doc, range.into())),
    );
}

pub(super) fn collect_use_sites(
    doc: Doc,
    name: NAbsName,
    locations: &mut Vec<Location>,
    symbols: &mut Symbols,
) {
    locations.extend(
        symbols
            .occurrences
            .use_sites
            .get(&name)
            .iter()
            .flat_map(|locations| locations.iter().map(|location| location.range()))
            .map(|range| Location::new(doc, range.into())),
    );
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

        let defs = lang_service.definitions(DOC, TPos16::from("foo"));
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
        let refs = lang_service.references(DOC, cursors[0].into(), true);
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
