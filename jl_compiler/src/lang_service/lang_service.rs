use super::{actions, doc_analysis::*};
use crate::{
    cps::*,
    logs::{LogItem, Logs},
    parse::PTree,
    source::{Doc, Loc, TPos16, TRange, TRange16},
    utils::VecArena,
};
use std::{
    collections::{HashMap, HashSet},
    mem::take,
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
    project_logs: Vec<LogItem>,
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

    pub(super) fn do_with_mods(
        &mut self,
        f: impl FnOnce(&mut LangService, &mut KModOutlines, &mut KModArena),
    ) {
        let doc_len = self.docs.len();

        let mut mod_docs: VecArena<KModTag, Doc> = VecArena::new();
        let mut mod_outlines = KModOutlines::new();
        let mut mods = KModArena::new();

        mod_docs.reserve(doc_len);
        mod_outlines.reserve(doc_len);
        mods.reserve(doc_len);

        // 各ドキュメントのデータをアリーナに移動する。
        for (&doc, doc_data) in self.docs.iter_mut() {
            let symbols = doc_data.request_symbols();

            let k_mod = mod_docs.alloc(doc);
            let mod_outline = take(&mut symbols.symbols.mod_outline);
            let mod_data = take(&mut symbols.symbols.mod_data);

            let k_mod2 = mod_outlines.alloc(mod_outline);
            let k_mod3 = mods.alloc(mod_data);
            assert_eq!(k_mod, k_mod2);
            assert_eq!(k_mod, k_mod3);

            doc_data.mod_opt = Some(k_mod);
        }

        f(self, &mut mod_outlines, &mut mods);

        // アリーナを解体する。
        for doc_data in self.docs.values_mut() {
            let k_mod = doc_data.mod_opt.take().unwrap();
            let symbols = doc_data.request_symbols();

            symbols.symbols.mod_outline = take(&mut k_mod.of_mut(&mut mod_outlines));
            symbols.symbols.mod_data = take(&mut k_mod.of_mut(&mut mods));
        }
    }

    pub(super) fn request_types(&mut self) -> &[LogItem] {
        if take(&mut self.dirty_sources).is_empty() {
            return &self.project_logs;
        }

        let logs = Logs::new();
        self.do_with_mods(|_, mod_outlines, mods| {
            for ((k_mod, mod_outline), mod_data) in mod_outlines.enumerate().zip(mods.iter_mut()) {
                resolve_types(k_mod, mod_outline, mod_data, &mod_outlines, logs.logger());
            }
        });

        self.project_logs = logs.finish();
        &self.project_logs
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
                mod_opt: None,
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

fn collect_symbols(doc: Doc, symbols: &Symbols, sites: &mut Sites) {
    fn on_symbol(symbol: KSymbol, parent: KLocalVarParent, sites: &mut Sites) {
        let kind = match symbol.cause {
            KSymbolCause::NameDef(..) => DefOrUse::Def,
            KSymbolCause::NameUse(..) => DefOrUse::Use,
            _ => return,
        };

        let (local_var, loc) = (symbol.local, symbol.cause.loc());
        let symbol = KModLocalSymbol::LocalVar { local_var, parent };
        sites.push((symbol, kind, loc));
    }

    fn on_params(params: &[KSymbol], parent: KLocalVarParent, sites: &mut Sites) {
        for symbol in params {
            on_symbol(*symbol, parent, sites);
        }
    }

    fn on_term(term: &KTerm, k_fn: KFn, sites: &mut Sites) {
        let (symbol, loc) = match *term {
            KTerm::Unit { .. }
            | KTerm::True { .. }
            | KTerm::False { .. }
            | KTerm::Int { .. }
            | KTerm::Float { .. }
            | KTerm::Char { .. }
            | KTerm::Str { .. } => return,
            KTerm::Name(symbol) => {
                on_symbol(symbol, KLocalVarParent::Fn(k_fn), sites);
                return;
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

    fn on_node(node: &KNode, k_fn: KFn, sites: &mut Sites) {
        for arg in &node.args {
            on_term(arg, k_fn, sites);
        }

        for result in &node.results {
            on_symbol(*result, KLocalVarParent::Fn(k_fn), sites);
        }

        for cont in &node.conts {
            on_node(cont, k_fn, sites);
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

        on_params(&fn_data.params, KLocalVarParent::Fn(k_fn), sites);

        for label_data in fn_data.labels.iter() {
            on_node(&label_data.body, k_fn, sites);
        }
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

        on_params(&data.params, KLocalVarParent::ExternFn(extern_fn), sites);
    }

    for (k_enum, outline) in symbols.mod_outline.enums.enumerate() {
        sites.push((KModLocalSymbol::Enum(k_enum), DefOrUse::Def, outline.loc));

        // バリアントの定義位置は consts や structs の定義時に収集される。
    }

    for (k_struct, outline) in symbols.mod_outline.structs.enumerate() {
        sites.push((
            KModLocalSymbol::Struct(k_struct),
            DefOrUse::Def,
            outline.loc,
        ));
    }

    // FIXME: fields

    for (ty, loc) in &symbols.ty_use_sites {
        // FIXME: alias, struct, never, etc.
        let symbol = match ty {
            KTy::Enum(k_enum) => KModLocalSymbol::Enum(*k_enum),
            KTy::Struct(k_struct) => KModLocalSymbol::Struct(*k_struct),
            _ => continue,
        };
        sites.push((symbol, DefOrUse::Use, Loc::new(doc, *loc)));
    }
}

pub(super) fn hit_test(
    doc: Doc,
    pos: TPos16,
    syntax: &Syntax,
    symbols: &Symbols,
) -> Option<(KModLocalSymbol, Location)> {
    let mut sites = vec![];
    collect_symbols(doc, symbols, &mut sites);

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
    collect_symbols(doc, symbols, &mut sites);

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
    collect_symbols(doc, symbols, &mut sites);

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
        let mut lang_service = new_service_from_str("fn f() -> i32 { \"\" }");
        let (_, errors) = lang_service.validate(DOC);
        assert_ne!(errors.len(), 0);
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
    fn test_references_enum_name() {
        let text = r#"
            enum <[A]> {}

            fn get_a() -> <$cursor|><[A]> {
                loop {}
            }

            fn consume_a(a: <[A]>) {
                let p: *<[A]> = &a;
                match a {}
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_const_variant() {
        let text = r#"
            enum IsGood {
                <[True]>,
                False,
            }

            fn a() {
                let is_good = <$cursor|><[IsGood::True]>;
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_record_variant() {
        let text = r#"
            enum I32Option {
                <[Some]> {
                    value: i32,
                },
                None,
            }

            enum I64Option {
                Some {
                    value: i64,
                },
                None,
            }

            fn some(value: i32) -> I32Option {
                <$cursor|><[I32Option::Some]> {
                    value: value,
                }
            }

            pub fn main() -> i32 {
                let is_some = match some(42) {
                    <[I32Option::Some]> { .. } => true,
                    I32Option::None => false,
                };

                let another = I64Option::Some { value: 84 };
                match another {
                    I64Option::Some { .. } => {}
                    I64Option::None => {}
                }
                0
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_record_struct() {
        let text = r#"
            extern fn print_sds(s1: *c8, d2: i32, s3: *c8);

            struct <[A]> {
                head: i32,
                tail: *<[A]>,
            }

            fn singleton(value: i32) -> <$cursor|><[A]> {
                <[A]> {
                    head: value,
                    tail: 0 as *<[A]>,
                }
            }

            fn go(a: *<[A]>) {
                if a as usize != 0 {
                    print_sds("", (*a).head, "\n");
                    go((*a).tail);
                }
            }
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_on_fn_with_name_missing() {
        let text = r#"
            <$cursor|><[fn ()]>
        "#;
        do_test_references(text);
    }

    #[test]
    fn test_references_no_hit_on_operator() {
        let text = r#"
            fn f() -> i32 {
                2 + 3 <$cursor|>* 5
            }
        "#;
        let cursor_text = parse_cursor_text(text).unwrap();
        let mut lang_service = new_service_from_str(cursor_text.as_str());
        let cursor_pos = cursor_text.to_pos_vec()[0];
        let refs = lang_service
            .references(DOC, cursor_pos.into(), true)
            .into_iter()
            .flatten()
            .map(|location| location.range())
            .collect::<Vec<_>>();
        assert_eq!(refs.len(), 0, "{:?}", refs);
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

    fn do_test_hover(text: &str, expected: Option<&str>) {
        let cursor_text = parse_cursor_text(text).unwrap();
        let mut lang_service = new_service_from_str(cursor_text.as_str());

        let pos = cursor_text.to_pos_vec()[0];
        let result = lang_service.hover(DOC, pos.into());
        assert_eq!(result.as_deref(), expected);
    }

    #[test]
    fn test_hover_param() {
        do_test_hover("fn f(x: i32) -> i32 { <|>x }", Some("i32"));
    }

    #[test]
    fn test_hover_local_var() {
        do_test_hover("fn f() { let x = 2_i64 + 3; <|>x; }", Some("i64"));
    }
}
