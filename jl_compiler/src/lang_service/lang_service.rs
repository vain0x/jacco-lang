use crate::{
    cps::{self, KOutlines, KRoot},
    front::{self, validate_syntax, NameResolution, Occurrences},
    logs::Logs,
    parse::{self, PRoot},
    source::{Doc, Pos, Range, TPos, TRange},
    token::{self, TokenSource},
};
use front::NName;
use log::{error, trace};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
    sync::Arc,
};

pub struct Location {
    doc: Doc,
    range: Range,
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
            validate_syntax(&root, logs.logger());

            let errors = logs_into_errors(&self.text, logs);

            Syntax { root, errors }
        };

        self.syntax_opt = Some(syntax);
        self.syntax_opt.as_mut().unwrap()
    }

    fn request_symbols(&mut self) -> &mut Symbols {
        if self.symbols_opt.is_some() {
            return self.symbols_opt.as_mut().unwrap();
        }

        let symbols = {
            let syntax = self.request_syntax();

            let logs = Logs::new();
            let res = front::resolve_name(&mut syntax.root, logs.logger());

            let res = Rc::new(res);
            let occurrences = {
                let res = res.clone();
                front::collect_occurrences(&syntax.root, res)
            };
            let res = Rc::try_unwrap(res).ok().unwrap();

            let errors = logs_into_errors(&self.text, logs);

            Symbols {
                name_resolution_opt: Some(res),
                occurrences,
                errors,
            }
        };

        self.symbols_opt = Some(symbols);
        self.symbols_opt.as_mut().unwrap()
    }

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

            let errors = logs_into_errors(&self.text, logs);

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

    pub fn definitions(&mut self, doc: Doc, pos: Pos) -> Vec<Location> {
        vec![]
    }

    pub fn document_highlight(&mut self, doc: Doc, pos: Pos) -> Option<(Vec<Range>, Vec<Range>)> {
        let symbols = self.request_symbols(doc)?;

        let name = symbols
            .occurrences
            .def_sites
            .iter()
            .chain(symbols.occurrences.use_sites.iter())
            .find_map(|(&name, locations)| {
                if locations
                    .iter()
                    .any(|location| location.range().contains_loosely(pos))
                {
                    Some(name)
                } else {
                    None
                }
            })?;

        let def_sites = symbols
            .occurrences
            .def_sites
            .get(&name)
            .iter()
            .flat_map(|locations| locations.iter().map(|location| location.range()))
            .collect();

        let use_sites = symbols
            .occurrences
            .use_sites
            .get(&name)
            .iter()
            .flat_map(|locations| locations.iter().map(|location| location.range()))
            .collect();

        Some((def_sites, use_sites))
    }

    pub fn hover(&mut self, doc: Doc, pos: Pos) -> Option<String> {
        let name = {
            let symbols = self.request_symbols(doc)?;
            symbols
                .occurrences
                .def_sites
                .iter()
                .chain(symbols.occurrences.use_sites.iter())
                .find_map(|(&name, locations)| {
                    if locations
                        .iter()
                        .any(|location| location.range().contains_loosely(pos))
                    {
                        Some(name)
                    } else {
                        None
                    }
                })?
        };

        let cps = self.request_cps(doc)?;

        match name {
            (NName::Fn(n_fn), NName::LocalVar(n_local_var)) => {
                let ty_env = &cps.root.fns[n_fn].ty_env;
                let ty = &cps.root.fns[n_fn].locals[n_local_var].ty;
                let enums = &cps.root.outlines.enums;
                let structs = &cps.root.outlines.structs;
                let ty_name = ty_env.display(ty, enums, structs);
                Some(ty_name)
            }
            _ => None,
        }
    }

    pub fn references(&mut self, doc: Doc, pos: Pos, include_definition: bool) -> Vec<Location> {
        vec![]
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

fn logs_into_errors(doc_text: &str, logs: Logs) -> Vec<(Range, String)> {
    logs.finish()
        .into_iter()
        .map(|item| {
            let t_range = {
                // 累積和を取っておくと効率がいい。
                let range = item.location.range;
                let start = TPos::from(&doc_text[..range.start_index()]);
                let end = start + TPos::from(&doc_text[range.start_index()..range.end_index()]);
                TRange::new(start, end)
            };
            (Range::from(t_range), item.message)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{Doc, LangService};

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
}
