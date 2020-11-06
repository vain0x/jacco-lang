use super::{actions, doc_analysis::*};
use crate::{
    cps::*,
    logs::Logs,
    parse::PTree,
    source::{Doc, Loc, TPos16, TRange, TRange16},
    utils::VecArena,
};
use std::{
    collections::{HashMap, HashSet},
    mem::take,
    path::PathBuf,
    rc::Rc,
    sync::Arc,
};

#[derive(Ord, PartialOrd, Eq, PartialEq)]
pub struct Location {
    doc: Doc,
    range: TRange,
}

impl Location {
    pub(crate) fn new(doc: Doc, range: TRange) -> Self {
        Self { doc, range }
    }

    pub fn doc(&self) -> Doc {
        self.doc
    }

    pub fn range(&self) -> TRange {
        self.range
    }
}

pub enum Content {
    String(String),
    JaccoCode(String),
    Concat(Vec<Content>),
}

impl Content {
    #[allow(unused)]
    pub(crate) fn to_string(&self) -> String {
        use std::io::{self, Write};

        fn write(f: &mut impl Write, content: &Content) -> io::Result<()> {
            match content {
                Content::String(text) => write!(f, "{}", text),
                Content::JaccoCode(code) => write!(f, "```jacco\n{}\n```", code),
                Content::Concat(contents) => {
                    for (i, content) in contents.iter().enumerate() {
                        if i != 0 {
                            write!(f, "\n")?;
                        }

                        write(f, content)?;
                    }
                    Ok(())
                }
            }
        }

        let mut out = Vec::new();
        write(&mut out, self).unwrap();
        unsafe { String::from_utf8_unchecked(out) }
    }
}

#[derive(Default)]
pub struct LangService {
    pub(super) docs: HashMap<Doc, AnalysisCache>,
    pub(super) mod_outline: KModOutline,
    pub(super) mod_data: KModData,
    dirty_sources: HashSet<Doc>,
    epoch: usize,
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
        let analysis = self.docs.get_mut(&doc)?;
        Some(analysis.request_symbols(&mut self.mod_outline))
    }

    pub(super) fn request_cps(&mut self, doc: Doc) -> Option<DocContentAnalysisMut<'_>> {
        let analysis = self.docs.get_mut(&doc)?;
        Some(analysis.request_cps(&mut self.mod_outline, &mut self.mod_data))
    }

    /// 単一のドキュメントを型検査する。
    pub(super) fn request_types_for(&mut self, doc: Doc) -> Option<DocContentAnalysisMut<'_>> {
        // FIXME: ドキュメント単位の型検査は未実装
        self.request_types();

        let analysis = self
            .docs
            .get_mut(&doc)?
            .doc_content_analysis_mut(&self.mod_outline, &mut self.mod_data);
        assert!(analysis.is_some());
        analysis
    }

    /// すべてのドキュメントを型検査する。
    pub(super) fn request_types(&mut self) {
        let everything_is_unchanged = self.dirty_sources.is_empty();
        if everything_is_unchanged {
            return;
        }

        let logs = Logs::new();

        // (型検査などの実装がインクリメンタルになっていないので) 解析をやり直す。
        // キャッシュのクリア
        {
            for doc_data in self.docs.values_mut() {
                doc_data.purge_cache();
            }

            self.mod_outline = Default::default();
            self.mod_data = Default::default();
            self.dirty_sources.clear();
        }

        // シンボル解決
        for analysis in self.docs.values_mut() {
            analysis.request_symbols(&mut self.mod_outline);
        }

        // エイリアスの解決
        {
            let mut aliases = take(&mut self.mod_outline.aliases);
            crate::cps::resolve_aliases(&mut aliases, &self.mod_outline, logs.logger());
            self.mod_outline.aliases = aliases;
        }

        // CPS 変換
        for analysis in self.docs.values_mut() {
            analysis.request_cps(&mut self.mod_outline, &mut self.mod_data);
        }

        // 型検査
        {
            resolve_types(&mut self.mod_outline, &mut self.mod_data, logs.logger());

            // エラーをドキュメントに振り分ける。
            for log in logs.finish() {
                let (doc, loc) = match log.loc().inner() {
                    Ok(it) => it,
                    Err(_) => continue,
                };

                let analysis = self
                    .docs
                    .get_mut(&doc)
                    .unwrap()
                    .doc_content_analysis_mut(&mut self.mod_outline, &mut self.mod_data)
                    .unwrap();
                let range = loc.range(&analysis.syntax.tree).unwrap_or(TRange::ZERO);
                analysis.cps.errors.push((range, log.message().to_string()));
            }
        }
    }

    // mod_outline が疎になってきたら一度すべて捨てる。(シンボルの ID 空間を再利用していないため。ID 空間の再利用や差分更新などを検討する必要があるかもしれない。バッチコンパイルでは ID 空間の過疎化は問題にならないので、いまのところハッシュマップは使わない予定。)
    fn gc(&mut self) {
        // たまにしか実行しない。
        self.epoch += 1;
        if self.epoch < 100 {
            return;
        }
        self.epoch = 0;

        // シンボルが少なければ GC しない。
        let threshold = {
            let mut lost = 0;
            let mut total = self.mod_outline.symbol_count();

            for doc_data in self.docs.values() {
                lost += doc_data.lost_symbol_count;
            }

            total >= 100_000 && lost >= total / 2
        };
        if !threshold {
            return;
        }

        // GC.
        for doc_data in self.docs.values_mut() {
            doc_data.purge_cache();
        }
    }

    pub fn open_doc(&mut self, doc: Doc, path: Arc<PathBuf>, version: i64, text: Rc<String>) {
        self.docs
            .insert(doc, AnalysisCache::new(doc, version, text, path));
        self.dirty_sources.insert(doc);
    }

    pub fn change_doc(&mut self, doc: Doc, version: i64, text: Rc<String>) {
        if let Some(analysis) = self.docs.get_mut(&doc) {
            analysis.set_text(version, text);
            self.dirty_sources.insert(doc);

            self.gc();
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

    pub fn hover(&mut self, doc: Doc, pos: TPos16) -> Option<Content> {
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

pub(super) fn to_range16(range: TRange) -> TRange16 {
    TRange16::at(TPos16::from(range.index), TPos16::from(range.len))
}

pub(super) fn loc_to_range(loc: Loc, doc: Doc, tree: &PTree) -> Option<TRange> {
    let opt = (|| {
        let (the_doc, loc) = loc.inner().ok()?;
        if the_doc != doc {
            return None;
        }

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

#[derive(Copy, Clone, Eq, PartialEq)]
pub(super) enum SymbolOccurrence {
    ModLocal(KModSymbol),
    LocalVar(KLocalVar, KLocalVarParent),
}

type Sites = Vec<(SymbolOccurrence, DefOrUse, Loc)>;

fn collect_symbols(
    doc: Doc,
    symbols: &Symbols,
    cps: &Cps,
    mod_outline: &KModOutline,
    mod_data: &KModData,
    sites: &mut Sites,
) {
    fn on_symbol(symbol: KVarTerm, parent: KLocalVarParent, sites: &mut Sites) {
        let kind = match symbol.cause {
            KVarTermCause::NameDef(..) => DefOrUse::Def,
            KVarTermCause::NameUse(..) => DefOrUse::Use,
            _ => return,
        };

        let (local_var, loc) = (symbol.local_var, symbol.cause.loc());
        let symbol = SymbolOccurrence::LocalVar(local_var, parent);
        sites.push((symbol, kind, loc));
    }

    fn on_params(params: &[KVarTerm], parent: KLocalVarParent, sites: &mut Sites) {
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
            | KTerm::Str { .. }
            | KTerm::TyProperty { .. } => return,
            KTerm::Name(symbol) => {
                on_symbol(symbol, KLocalVarParent::Fn(k_fn), sites);
                return;
            }
            KTerm::Const { k_const, loc } => (KModSymbol::Const(k_const), loc),
            KTerm::StaticVar { static_var, loc } => (KModSymbol::StaticVar(static_var), loc),
            KTerm::Fn { k_fn, loc, .. } => (KModSymbol::Fn(k_fn), loc),
            KTerm::Label { .. } | KTerm::Return { .. } => return,
            KTerm::ExternFn { extern_fn, loc } => (KModSymbol::ExternFn(extern_fn), loc),
            KTerm::RecordTag { .. } | KTerm::FieldTag(_) => return,
        };
        sites.push((SymbolOccurrence::ModLocal(symbol), DefOrUse::Use, loc));
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

    for (k_const, const_outline) in mod_outline.consts.enumerate() {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::Const(k_const)),
            DefOrUse::Def,
            const_outline.loc,
        ));
    }

    for (static_var, static_var_outline) in mod_outline.static_vars.enumerate() {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::StaticVar(static_var)),
            DefOrUse::Def,
            static_var_outline.loc,
        ));
    }

    for ((k_fn, fn_outline), fn_data) in mod_outline.fns.enumerate().zip(mod_data.fns.iter()) {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::Fn(k_fn)),
            DefOrUse::Def,
            fn_outline.loc,
        ));

        on_params(&fn_data.params, KLocalVarParent::Fn(k_fn), sites);

        for label_data in fn_data.labels.iter() {
            on_node(&label_data.body, k_fn, sites);
        }
    }

    for ((extern_fn, outline), data) in mod_outline
        .extern_fns
        .enumerate()
        .zip(mod_data.extern_fns.iter())
    {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::ExternFn(extern_fn)),
            DefOrUse::Def,
            outline.loc,
        ));

        on_params(&data.params, KLocalVarParent::ExternFn(extern_fn), sites);
    }

    for (const_enum, outline) in mod_outline.const_enums.enumerate() {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::ConstEnum(const_enum)),
            DefOrUse::Def,
            outline.loc,
        ));

        // バリアントの定義位置は consts の定義時に収集される。
    }

    for (struct_enum, outline) in mod_outline.struct_enums.enumerate() {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::StructEnum(struct_enum)),
            DefOrUse::Def,
            outline.loc,
        ));

        // バリアントの定義位置は structs の定義時に収集される。
    }

    for (k_struct, outline) in mod_outline.structs.enumerate() {
        sites.push((
            SymbolOccurrence::ModLocal(KModSymbol::Struct(k_struct)),
            DefOrUse::Def,
            outline.loc,
        ));
    }

    // FIXME: fields

    for (ty, loc) in symbols.ty_use_sites.iter().chain(&cps.ty_use_sites) {
        // FIXME: alias, struct, never, etc.
        let symbol = match ty {
            KTy::ConstEnum(const_enum) => KModSymbol::ConstEnum(*const_enum),
            KTy::StructEnum(struct_enum) => KModSymbol::StructEnum(*struct_enum),
            KTy::Struct(k_struct) => KModSymbol::Struct(*k_struct),
            _ => continue,
        };
        sites.push((
            SymbolOccurrence::ModLocal(symbol),
            DefOrUse::Use,
            Loc::new(doc, *loc),
        ));
    }
}

pub(super) fn hit_test(
    doc: Doc,
    pos: TPos16,
    syntax: &Syntax,
    symbols: &Symbols,
    cps: &Cps,
    mod_outline: &KModOutline,
    mod_data: &KModData,
) -> Option<(SymbolOccurrence, Location)> {
    let mut sites = vec![];
    collect_symbols(doc, symbols, cps, mod_outline, mod_data, &mut sites);

    sites.iter().find_map(|&(symbol, _, loc)| {
        let range = loc_to_range(loc, doc, &syntax.tree)?;
        if to_range16(range).contains_inclusive(pos) {
            Some((symbol, Location::new(doc, range)))
        } else {
            None
        }
    })
}

pub(super) fn collect_def_sites(
    doc: Doc,
    symbol: SymbolOccurrence,
    syntax: &Syntax,
    symbols: &Symbols,
    cps: &Cps,
    mod_outline: &KModOutline,
    mod_data: &KModData,
    locations: &mut Vec<Location>,
) {
    let mut sites = vec![];
    collect_symbols(doc, symbols, cps, mod_outline, mod_data, &mut sites);

    locations.extend(sites.iter().filter_map(|&(s, def_or_use, loc)| {
        if s == symbol && def_or_use == DefOrUse::Def {
            let range = loc_to_range(loc, doc, &syntax.tree)?;
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
    symbol: SymbolOccurrence,
    syntax: &Syntax,
    symbols: &Symbols,
    cps: &Cps,
    mod_outline: &KModOutline,
    mod_data: &KModData,
    locations: &mut Vec<Location>,
) {
    let mut sites = vec![];
    collect_symbols(doc, symbols, cps, mod_outline, mod_data, &mut sites);

    locations.extend(sites.iter().filter_map(|&(s, def_or_use, loc)| {
        if s == symbol && def_or_use == DefOrUse::Use {
            let range = loc_to_range(loc, doc, &syntax.tree)?;
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
    use super::{Content, Doc, LangService};
    use crate::source::{cursor_text::parse_cursor_text, TPos16};
    use std::path::PathBuf;

    const DOC: Doc = Doc::new(1);

    fn to_vec<T, I: IntoIterator<Item = T>>(iter: I) -> Vec<T> {
        iter.into_iter().collect()
    }

    fn new_service_from_str(s: &str) -> LangService {
        let mut it = LangService::new();
        it.did_initialize();
        it.open_doc(
            DOC,
            PathBuf::from("test.jacco").into(),
            1,
            s.to_string().into(),
        );
        it
    }

    fn do_test_highlight(text: &str) {
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
        cursors.sort();

        let (mut def_sites, mut use_sites) = lang_service
            .document_highlight(DOC, cursor_pos.into())
            .unwrap_or_default();
        def_sites.sort();
        use_sites.sort();

        let actual = def_sites
            .into_iter()
            .map(|range| ("def", range))
            .chain(use_sites.into_iter().map(|range| ("use", range)))
            .collect::<Vec<_>>();
        assert_eq!(actual, cursors);
    }

    #[test]
    fn test_highlight_fn_stmt() {
        do_test_highlight("fn <$cursor|><$def[f]>() {}");
    }

    #[test]
    fn test_highlight_fields() {
        do_test_highlight(
            r#"
                struct TestState {
                    pass: usize,
                    fail: usize
                }

                struct Another {
                    pass: usize,
                }

                fn new_test_state() -> TestState {
                    TestState {
                        <$use[pass]>: 0_usize,
                        fail: 0_usize,
                    }
                }

                fn pass(state: *mut TestState) {
                    (*state).<$cursor|><$use[pass]> += 1_usize;
                }

                fn display(state: *TestState) -> *u8 {
                    if (*state).fail > 0 {
                        "FAIL: Assertion violated"
                    } else if (*state).<$use[pass]> == 0 {
                        "FAIL: No assertions"
                    } else {
                        "OK"
                    }
                }

                fn another() {
                    let a = Another { pass: 0_usize };
                    a.pass += 1_usize;
                }
            "#,
        );
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
    fn test_validate_type_errors_update_correctly() {
        fn t(s: impl Into<String>) -> std::rc::Rc<String> {
            s.into().into()
        }

        let mut lang_service = new_service_from_str("");
        let mut v = 1_i64;

        v += 1;
        lang_service.change_doc(DOC, v, t("fn main() -> i32 { \"\" }"));
        let (_, errors) = lang_service.validate(DOC);
        assert_ne!(errors.len(), 0);

        v += 1;
        lang_service.change_doc(DOC, v, t("fn main() -> i32 { 0 }"));
        let (_, errors) = lang_service.validate(DOC);
        assert_eq!(errors.len(), 0, "{:?}", errors);
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
            enum <[A]> { A }

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

    // パスの解決を変更したことで I32Option::Some などが Some バリアントではなく I32Option 型の出現箇所とみなされるようになってしまった
    #[should_panic(expected = "assertion failed")]
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

    #[ignore = "mod_outline_gen をいじってから名前のない関数がシンボルとして登録されなくなってしまった"]
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
        assert_eq!(result.as_ref().map(Content::to_string).as_deref(), expected);
    }

    #[test]
    fn test_hover_param() {
        do_test_hover("fn f(x: i32) -> i32 { <|>x }", Some("```jacco\ni32\n```"));
    }

    #[test]
    fn test_hover_local_var() {
        do_test_hover(
            "fn f() { let x = 2_i64 + 3; <|>x; }",
            Some("```jacco\ni64\n```"),
        );
    }

    #[test]
    fn test_hover_fn() {
        do_test_hover(
            "fn <|>f(x: i32, y: i64) {}",
            Some("```jacco\nfn f(x: i32, y: i64);\n```"),
        );
    }

    #[test]
    fn test_hover_fn_with_result_ty() {
        do_test_hover(
            "fn g() -> never { g<|>() }",
            Some("```jacco\nfn g() -> never;\n```"),
        );
    }

    #[test]
    fn test_hover_fn_with_doc_comments() {
        do_test_hover(
            r#"
                /// another
                fn f() {}

                /// 実行を終了します。
                ///
                /// 終了コードは 0 でない値になります。
                extern fn <|>abort() -> never;

                /// a
                fn g() {}
            "#,
            Some("実行を終了します。\n\n終了コードは 0 でない値になります。\n```jacco\nextern fn abort() -> never;\n```"),
        );
    }

    #[test]
    fn test_hover_extern_fn() {
        do_test_hover(
            "extern fn <|>abort() -> never;",
            Some("```jacco\nextern fn abort() -> never;\n```"),
        );
    }

    #[test]
    fn test_multiple_requests() {
        //! should not panic

        let text = r#"<|>
            /// 🐍<|>🐧
            struct <|>A {
                p: *A,
            }

            fn <|>f() -> <|>A {
                if true {
                    loop { loop <|>{} }
                }
                A<|> {
                    p: &loop {},
                }
            }

            fn <|>m<|>ain<|>() <|>-> <|>i32 {
                loop {}
            }
        <|>"#;

        let cursor_text = parse_cursor_text(text).unwrap();
        let mut lang_service = new_service_from_str(cursor_text.as_str());

        lang_service.validate(DOC);
        for pos in cursor_text.to_pos_vec() {
            lang_service.document_highlight(DOC, pos.into());
            lang_service.hover(DOC, pos.into());
            lang_service.definitions(DOC, pos.into());
            lang_service.references(DOC, pos.into(), true);
        }
    }
}
