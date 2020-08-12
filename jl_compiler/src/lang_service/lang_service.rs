use super::{actions, doc_analysis::*};
use crate::{
    cps::*,
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

type Sites = Vec<(KModLocalSymbol, DefOrUse, Loc)>;

fn collect_symbols(symbols: &Symbols, sites: &mut Sites) {
    fn on_term(term: &KTerm, k_fn: KFn, sites: &mut Sites) {
        let (symbol, loc) = match *term {
            KTerm::Unit { .. }
            | KTerm::True { .. }
            | KTerm::False { .. }
            | KTerm::Int { .. }
            | KTerm::Float { .. }
            | KTerm::Char { .. }
            | KTerm::Str { .. } => return,
            KTerm::Name(KSymbol { local, cause }) => {
                let local_var = KModLocalSymbol::LocalVar {
                    local_var: local,
                    parent: KLocalVarParent::Fn(k_fn),
                };
                (local_var, cause.loc())
            }
            KTerm::Alias { alias, loc } => (KModLocalSymbol::Alias(alias), loc),
            KTerm::Const { k_const, loc } => (KModLocalSymbol::Const(k_const), loc),
            KTerm::StaticVar { static_var, loc } => (KModLocalSymbol::StaticVar(static_var), loc),
            KTerm::Fn { k_fn, loc } => (KModLocalSymbol::Fn(k_fn), loc),
            KTerm::Label { .. } | KTerm::Return { .. } => return,
            KTerm::ExternFn { extern_fn, loc } => (KModLocalSymbol::ExternFn(extern_fn), loc),
            KTerm::RecordTag { .. } | KTerm::FieldTag(_) => return,
        };
        sites.push((symbol, DefOrUse::Use, loc));
    }

    fn collect_local_vars(local_vars: &KLocalArena, parent: KLocalVarParent, sites: &mut Sites) {
        for (local_var, local_var_data) in local_vars.enumerate() {
            let symbol = KModLocalSymbol::LocalVar { local_var, parent };
            sites.push((symbol, DefOrUse::Use, local_var_data.loc));
        }
    }

    fn go(node: &KNode, k_fn: KFn, sites: &mut Sites) {
        for arg in &node.args {
            on_term(arg, k_fn, sites);
        }

        for cont in &node.conts {
            go(cont, k_fn, sites);
        }
    }

    for (k_const, const_data) in symbols.mod_outline.consts.enumerate() {
        sites.push((
            KModLocalSymbol::Const(k_const),
            DefOrUse::Def,
            const_data.loc,
        ));
    }

    for (static_var, static_var_data) in symbols.mod_outline.static_vars.enumerate() {
        sites.push((
            KModLocalSymbol::StaticVar(static_var),
            DefOrUse::Def,
            static_var_data.loc,
        ));
    }

    for ((k_fn, fn_outline), fn_data) in symbols
        .mod_outline
        .fns
        .enumerate()
        .zip(symbols.mod_data.fns.iter())
    {
        sites.push((KModLocalSymbol::Fn(k_fn), DefOrUse::Def, fn_outline.loc));

        for label_data in fn_data.labels.iter() {
            go(&label_data.body, k_fn, sites);
        }

        collect_local_vars(&fn_data.locals, KLocalVarParent::Fn(k_fn), sites);
    }

    for ((extern_fn, outline), data) in symbols
        .mod_outline
        .extern_fns
        .enumerate()
        .zip(symbols.mod_data.extern_fns.iter())
    {
        sites.push((
            KModLocalSymbol::ExternFn(extern_fn),
            DefOrUse::Def,
            outline.loc,
        ));

        collect_local_vars(&data.locals, KLocalVarParent::ExternFn(extern_fn), sites);
    }

    // enum, structs, fields
}

pub(super) fn hit_test(
    doc: Doc,
    pos: TPos16,
    syntax: &Syntax,
    symbols: &Symbols,
) -> Option<(KModLocalSymbol, Location)> {
    let mut sites = vec![];
    collect_symbols(symbols, &mut sites);

    sites.iter().find_map(|&(symbol, _, loc)| {
        let range = loc_to_range(loc, &syntax.tree)?;
        if to_range16(range).contains_inclusive(pos) {
            Some((symbol, Location::new(doc, range)))
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
    collect_symbols(symbols, &mut sites);

    locations.extend(sites.iter().filter_map(|&(s, def_or_use, loc)| {
        if s == symbol && def_or_use == DefOrUse::Def {
            let range = loc_to_range(loc, &syntax.tree)?;
            Some(Location::new(doc, range))
        } else {
            None
        }
    }));

    locations.sort();
    locations.dedup();
}

pub(super) fn collect_use_sites(
    doc: Doc,
    symbol: KModLocalSymbol,
    syntax: &Syntax,
    symbols: &Symbols,
    locations: &mut Vec<Location>,
) {
    let mut sites = vec![];
    collect_symbols(symbols, &mut sites);

    locations.extend(sites.iter().filter_map(|&(s, def_or_use, loc)| {
        if s == symbol && def_or_use == DefOrUse::Use {
            let range = loc_to_range(loc, &syntax.tree)?;
            Some(Location::new(doc, range))
        } else {
            None
        }
    }));

    locations.sort();
    locations.dedup();
}

#[cfg(test)]
mod tests {
    use super::{Doc, LangService};
    use crate::source::{cursor_text::parse_cursor_text, TPos16};

    const DOC: Doc = Doc::new(1);

    fn to_vec<T, I: IntoIterator<Item = T>>(iter: I) -> Vec<T> {
        iter.into_iter().collect()
    }

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

    fn do_test_references(text: &str) {
        let cursor_text = parse_cursor_text(text).unwrap();
        let mut lang_service = new_service_from_str(cursor_text.as_str());

        let mut cursors = cursor_text.to_range_assoc();
        let cursor_pos = match cursors.iter().position(|&(name, _)| name == "cursor") {
            Some(i) => {
                let (_, range) = cursors.remove(i);
                range.start()
            }
            None => panic!(
                "テキストに参照検索の基準となる位置を表すマーカー <$cursor|> が含まれていません。"
            ),
        };

        // 入力されたテキストを検証する。
        assert_ne!(cursors.len(), 0, "参照の範囲を表すマーカーがありません");
        let expected = cursors.into_iter().map(|(_, range)| range);

        let refs = lang_service.references(DOC, cursor_pos.into(), true);
        let actual = refs.into_iter().flatten().map(|location| location.range);
        assert_eq!(to_vec(actual), to_vec(expected));
    }

    #[test]
    fn test_references_const() {
        let text = r#"
            const <$cursor|><[PI]>: f64 = 3.14;

            fn f() -> f64 {
                <[PI]>
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_static() {
        let text = r#"
            fn fresh_id() -> i64 {
                static <$cursor|><[LAST_ID]>: i64 = 0_i64;
                <[LAST_ID]> += 1;
                <[LAST_ID]>
            }

            fn other() {
                static LAST_ID: u64 = 0_u64;
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_fn() {
        let text = r#"
            fn <$cursor|><[f]>() {
                <[f]>(<[f]>());

                // shadowing
                {
                    fn f() {}
                    f();
                }

                // ブロックの中
                {
                    <[f]>();
                }
            }

            fn g() {
                g();
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_param() {
        let text = r#"
            fn f(<[a]>: i32, b: i32) {
                f(<$cursor|><[a]>);
                <[a]> += 1;

                // shadowing
                {
                    let a = 1;
                    f(a);
                }

                // ブロックの中
                {
                    g(&mut <[a]>);
                }
            }

            fn g(a: i64) {}
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_extern_fn() {
        let text = r#"
            extern fn <$cursor|><[abort]>() -> never;

            fn f() {
                <[abort]>()
            }
        "#;
        do_test_references(text);
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
