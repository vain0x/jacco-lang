use crate::{
    cli::MyLocResolver,
    cps::{self, KOutlines, KRoot},
    front::{self, validate_syntax, NameResolution, Occurrences},
    logs::Logs,
    parse::{self, PRoot},
    source::{loc::LocResolver, Doc, Loc, Pos, Range, TPos, TRange},
    token::{self, TokenSource},
};
use cps::{KTy, KTyEnv};
use front::{NAbsName, NName, NParentFn};
use log::{error, trace};
use parse::PToken;
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

#[derive(Copy, Clone, Debug)]
pub enum DefOrUse {
    Def,
    Use,
}

pub struct Location {
    doc: Doc,
    range: Range,
}

impl Location {
    fn new(doc: Doc, range: Range) -> Self {
        Self { doc, range }
    }

    pub fn range(&self) -> Range {
        self.range
    }
}

struct Syntax {
    root: PRoot,
    errors: Vec<(Range, String)>,
}

struct Symbols {
    name_resolution_opt: Option<NameResolution>,
    occurrences: Occurrences,
    errors: Vec<(Range, String)>,
}

struct Cps {
    root: KRoot,
    errors: Vec<(Range, String)>,
}

struct AnalysisCache {
    doc: Doc,
    version: i64,
    text: Rc<String>,
    source_path: Arc<PathBuf>,
    syntax_opt: Option<Syntax>,
    symbols_opt: Option<Symbols>,
    cps_opt: Option<Cps>,
}

impl LocResolver for AnalysisCache {
    fn doc_path(&self, doc: Doc) -> Option<&Path> {
        Some(self.source_path.as_ref())
    }

    fn token_range(&self, doc: Doc, token: PToken) -> TRange {
        match &self.syntax_opt {
            Some(syntax) => token.location(&syntax.root.tokens).range().into(),
            None => TPos::ZERO.to_empty_range(),
        }
    }
}

impl AnalysisCache {
    fn set_text(&mut self, version: i64, text: Rc<String>) {
        self.version = version;
        self.text = text;
        self.syntax_opt = None;
        self.symbols_opt = None;
        self.cps_opt = None;
    }

    fn request_syntax(&mut self) -> &mut Syntax {
        if self.syntax_opt.is_some() {
            return self.syntax_opt.as_mut().unwrap();
        }

        let tokens = {
            Doc::set_path(self.doc, &self.source_path);
            let token_source = TokenSource::File(self.doc);
            let source_code = self.text.clone();
            token::tokenize(token_source, source_code)
        };
        let syntax = {
            let logs = Logs::new();

            let root = parse::parse_tokens(tokens, logs.logger());
            let errors = validate_syntax(&root);
            let logger = logs.logger();
            for (loc, message) in errors {
                let range = loc.resolve(&root);
                logger.error((self.doc.into(), range.into()), message);
            }
            let errors = logs_into_errors(logs, self);

            Syntax { root, errors }
        };

        self.syntax_opt = Some(syntax);
        self.syntax_opt.as_mut().unwrap()
    }

    fn request_symbols(&mut self) -> &mut Symbols {
        if self.symbols_opt.is_some() {
            return self.symbols_opt.as_mut().unwrap();
        }

        let doc = self.doc;
        let symbols = {
            let syntax = self.request_syntax();

            let logs = Logs::new();
            let (res, errors) = front::resolve_name(&mut syntax.root);
            {
                let logger = logs.logger();
                for (loc, message) in errors {
                    let range = loc.resolve(&syntax.root);
                    logger.error((doc, range.into()), message);
                }
            }

            let res = Rc::new(res);
            let occurrences = {
                let res = res.clone();
                front::collect_occurrences(&syntax.root, res)
            };
            let res = Rc::try_unwrap(res).ok().unwrap();

            let errors = logs_into_errors(logs, self);

            Symbols {
                name_resolution_opt: Some(res),
                occurrences,
                errors,
            }
        };

        self.symbols_opt = Some(symbols);
        self.symbols_opt.as_mut().unwrap()
    }

    /// FIXME: rename to request_typed_cps?
    fn request_cps(&mut self) -> &mut Cps {
        if self.cps_opt.is_some() {
            return self.cps_opt.as_mut().unwrap();
        }

        let cps = {
            self.request_symbols();
            let syntax = self.syntax_opt.as_mut().unwrap();
            let symbols = self.symbols_opt.as_mut().unwrap();

            let logs = Logs::new();
            let mut root = front::cps_conversion(
                &syntax.root,
                symbols.name_resolution_opt.as_ref().unwrap(),
                logs.logger(),
            );

            cps::resolve_types(&mut root, logs.logger());

            let errors = logs_into_errors(logs, self);

            Cps { root, errors }
        };

        self.cps_opt = Some(cps);
        self.cps_opt.as_mut().unwrap()
    }
}

#[derive(Default)]
pub struct LangService {
    docs: HashMap<Doc, AnalysisCache>,
    dirty_sources: HashSet<Doc>,
}

impl LangService {
    pub fn new() -> Self {
        LangService::default()
    }

    pub fn did_initialize(&mut self) {}

    pub fn shutdown(&mut self) {}

    fn request_syntax(&mut self, doc: Doc) -> Option<&mut Syntax> {
        self.docs.get_mut(&doc).map(|cache| cache.request_syntax())
    }

    fn request_symbols(&mut self, doc: Doc) -> Option<&mut Symbols> {
        self.docs.get_mut(&doc).map(|cache| cache.request_symbols())
    }

    fn request_cps(&mut self, doc: Doc) -> Option<&mut Cps> {
        self.docs.get_mut(&doc).map(|cache| cache.request_cps())
    }

    fn hit_test(&mut self, doc: Doc, pos: Pos) -> Option<(NAbsName, Location)> {
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

    pub fn completion(&mut self, doc: Doc, pos: Pos) -> Vec<()> {
        vec![]
    }

    // references と同様
    pub fn definitions(&mut self, doc: Doc, pos: Pos) -> Option<Vec<Location>> {
        let symbols = self.request_symbols(doc)?;

        let (name, _) = hit_test(doc, pos, symbols)?;
        let mut def_sites = vec![];
        collect_def_sites(doc, name, &mut def_sites, symbols);

        Some(def_sites)
    }

    pub fn document_highlight(&mut self, doc: Doc, pos: Pos) -> Option<(Vec<Range>, Vec<Range>)> {
        let symbols = self.request_symbols(doc)?;

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

    pub fn hover(&mut self, doc: Doc, pos: Pos) -> Option<String> {
        let (name, _) = {
            let symbols = self.request_symbols(doc)?;
            hit_test(doc, pos, symbols)?
        };

        let cps = self.request_cps(doc)?;
        let ty = name.ty(&cps.root);
        let ty_env = name.ty_env(&cps.root);
        Some(display_ty(ty, ty_env, &cps.root))
    }

    pub fn references(
        &mut self,
        doc: Doc,
        pos: Pos,
        include_definition: bool,
    ) -> Option<Vec<(DefOrUse, Location)>> {
        let symbols = self.request_symbols(doc)?;

        let (name, _) = hit_test(doc, pos, symbols)?;
        let mut references = vec![];
        let mut locations = vec![];

        if include_definition {
            collect_def_sites(doc, name, &mut locations, symbols);
            references.extend(
                locations
                    .drain(..)
                    .into_iter()
                    .map(|location| (DefOrUse::Def, Location::new(doc, location.range()))),
            );
        }

        collect_use_sites(doc, name, &mut locations, symbols);
        references.extend(
            locations
                .drain(..)
                .map(|location| (DefOrUse::Use, Location::new(doc, location.range()))),
        );

        Some(references)
    }

    pub fn prepare_rename(&mut self, doc: Doc, pos: Pos) -> Option<()> {
        None
    }

    pub fn rename(&mut self, doc: Doc, pos: Pos, new_name: String) -> Option<()> {
        None
    }

    pub fn validate(&mut self, doc: Doc) -> (Option<i64>, Vec<(Range, String)>) {
        self.docs
            .get_mut(&doc)
            .map(|analysis| {
                let version_opt = Some(analysis.version);

                let mut errors = analysis.request_syntax().errors.clone();
                if errors.is_empty() {
                    errors.extend(analysis.request_symbols().errors.clone());
                }
                if errors.is_empty() {
                    errors.extend(analysis.request_cps().errors.clone());
                }

                (version_opt, errors)
            })
            .unwrap_or((None, vec![]))
    }
}

fn logs_into_errors(logs: Logs, resolver: &impl LocResolver) -> Vec<(Range, String)> {
    logs.finish()
        .into_iter()
        .map(|item| {
            let (message, _, range) = item.resolve(resolver);
            (range.into(), message.to_string())
        })
        .collect()
}

impl NParentFn {
    fn ty_env(self, k_root: &KRoot) -> &KTyEnv {
        match self {
            NParentFn::Fn(k_fn) => &k_fn.of(&k_root.fns).ty_env,
            NParentFn::ExternFn(extern_fn) => KTyEnv::EMPTY,
        }
    }
}

impl NAbsName {
    pub(crate) fn ty(self, k_root: &KRoot) -> &KTy {
        let name = match self {
            NAbsName::Unresolved => return &KTy::Unresolved,
            NAbsName::LocalVar {
                parent_fn: NParentFn::Fn(k_fn),
                local,
            } => return &local.of(&k_fn.of(&k_root.fns).locals).ty,
            NAbsName::LocalVar {
                parent_fn: NParentFn::ExternFn(extern_fn),
                local,
            } => return &local.of(&extern_fn.of(&k_root.extern_fns).locals).ty,
            NAbsName::Other(name) => name,
        };

        &KTy::Unresolved
    }

    pub(crate) fn ty_env(self, k_root: &KRoot) -> &KTyEnv {
        match self {
            NAbsName::LocalVar { parent_fn, .. } => parent_fn.ty_env(k_root),
            NAbsName::Other(NName::Fn(k_fn)) => &k_fn.of(&k_root.fns).ty_env,
            _ => KTyEnv::EMPTY,
        }
    }
}

fn hit_test(doc: Doc, pos: Pos, symbols: &Symbols) -> Option<(NAbsName, Location)> {
    symbols
        .occurrences
        .def_sites
        .iter()
        .chain(symbols.occurrences.use_sites.iter())
        .find_map(|(&name, locations)| {
            locations.iter().find_map(|&location| {
                if location.range().contains_loosely(pos) {
                    Some((
                        name,
                        Location {
                            doc,
                            range: location.range(),
                        },
                    ))
                } else {
                    None
                }
            })
        })
}

fn collect_def_sites(
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
            .map(|range| Location::new(doc, range)),
    );
}

fn collect_use_sites(
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
            .map(|range| Location::new(doc, range)),
    );
}

fn display_ty(ty: &KTy, ty_env: &KTyEnv, k_root: &KRoot) -> String {
    let enums = &k_root.outlines.enums;
    let structs = &k_root.outlines.structs;
    ty_env.display(ty, enums, structs)
}

#[cfg(test)]
mod tests {
    use super::{Doc, LangService};
    use crate::source::{cursor_text::parse_cursor_text, Pos};

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
        let mut lang_service = new_service_from_str("fn f() { 2_i32 + 3_f64 }");
        let (_, errors) = lang_service.validate(DOC);
        assert_ne!(errors.len(), 0);
    }

    #[test]
    fn test_definition() {
        let mut lang_service = new_service_from_str("fn foo() { foo(); }");

        let defs = lang_service.definitions(DOC, Pos::new(0, 3, 3));
        assert_eq!(
            defs.unwrap()
                .into_iter()
                .map(|location| format!("{:?}", location.range()))
                .collect::<Vec<_>>()
                .join("; "),
            "1.4-1.7"
        );

        let defs = lang_service.definitions(DOC, Pos::new(0, 14, 14));
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
            refs.unwrap()
                .into_iter()
                .map(|(def_or_use, location)| format!("{:?} {:?}", def_or_use, location.range()))
                .collect::<Vec<_>>()
                .join("; "),
            "Def 3.21-3.24; Use 4.17-4.20; Use 5.17-5.20"
        );
    }
}
