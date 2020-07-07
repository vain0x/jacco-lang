use crate::{
    cps::{self, KOutlines, KRoot},
    front::{self, validate_syntax, NameResolution, Occurrences},
    logs::Logs,
    parse::{self, PRoot},
    source::{Pos, Range, SourceFile},
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

pub type Source = usize;

pub struct Location {
    source: Source,
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

#[derive(Default)]
struct AnalysisCache {
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
            let source_file = SourceFile {
                source_path: self.source_path.clone(),
            };
            let token_source = TokenSource::File(source_file);
            let source_code = self.text.clone();
            token::tokenize(token_source, source_code)
        };
        let syntax = {
            let logs = Logs::new();

            let root = parse::parse_tokens(tokens, logs.logger());
            validate_syntax(&root, logs.logger());

            let log_items = logs.finish();
            let errors = log_items
                .into_iter()
                .map(|log_item| (log_item.location.range(), log_item.message))
                .collect();

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

            let log_items = logs.finish();
            let errors = log_items
                .into_iter()
                .map(|log_item| (log_item.location.range(), log_item.message))
                .collect();

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

            let log_items = logs.finish();
            let errors = log_items
                .into_iter()
                .map(|log_item| (log_item.location.range(), log_item.message))
                .collect();

            Cps { root, errors }
        };

        self.cps_opt = Some(cps);
        self.cps_opt.as_mut().unwrap()
    }
}

#[derive(Default)]
pub struct LangService {
    sources: HashMap<Source, AnalysisCache>,
    dirty_sources: HashSet<Source>,
}

impl LangService {
    pub fn new() -> Self {
        LangService::default()
    }

    pub fn did_initialize(&mut self) {}

    pub fn shutdown(&mut self) {}

    fn request_syntax(&mut self, source: Source) -> Option<&mut Syntax> {
        self.sources
            .get_mut(&source)
            .map(|cache| cache.request_syntax())
    }

    fn request_symbols(&mut self, source: Source) -> Option<&mut Symbols> {
        self.sources
            .get_mut(&source)
            .map(|cache| cache.request_symbols())
    }

    fn request_cps(&mut self, source: Source) -> Option<&mut Cps> {
        self.sources
            .get_mut(&source)
            .map(|cache| cache.request_cps())
    }

    pub fn open_doc(&mut self, source: Source, version: i64, text: Rc<String>) {
        self.sources.insert(
            source,
            AnalysisCache {
                version,
                text,
                // FIXME: 引数でもらう
                source_path: PathBuf::from("main.jacco").into(),
                syntax_opt: None,
                symbols_opt: None,
                cps_opt: None,
            },
        );
        self.dirty_sources.insert(source);
    }

    pub fn change_doc(&mut self, source: Source, version: i64, text: Rc<String>) {
        if let Some(analysis) = self.sources.get_mut(&source) {
            analysis.set_text(version, text);
            self.dirty_sources.insert(source);
        }
    }

    pub fn close_doc(&mut self, source: Source) {
        self.sources.remove(&source);
        self.dirty_sources.remove(&source);
    }

    pub fn completion(&mut self, source: Source, pos: Pos) -> Vec<()> {
        vec![]
    }

    pub fn definitions(&mut self, source: Source, pos: Pos) -> Vec<Location> {
        vec![]
    }

    pub fn document_highlight(
        &mut self,
        source: Source,
        pos: Pos,
    ) -> Option<(Vec<Range>, Vec<Range>)> {
        let symbols = self.request_symbols(source)?;

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

    pub fn hover(&mut self, source: Source, pos: Pos) -> Option<String> {
        let name = {
            let symbols = self.request_symbols(source)?;
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

        let cps = self.request_cps(source)?;

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

    pub fn references(
        &mut self,
        source: Source,
        pos: Pos,
        include_definition: bool,
    ) -> Vec<Location> {
        vec![]
    }

    pub fn prepare_rename(&mut self, source: Source, pos: Pos) -> Option<()> {
        None
    }

    pub fn rename(&mut self, source: Source, pos: Pos, new_name: String) -> Option<()> {
        None
    }

    pub fn validate(&mut self, source: Source) -> (Option<i64>, Vec<(Range, String)>) {
        self.sources
            .get_mut(&source)
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

#[cfg(test)]
mod tests {
    use super::{LangService, Source};

    const SOURCE: Source = 1;

    fn new_service_from_str(s: &str) -> LangService {
        let mut it = LangService::new();
        it.did_initialize();
        it.open_doc(SOURCE, 1, s.to_string().into());
        it
    }

    #[test]
    fn test_validate_good() {
        let mut lang_service = new_service_from_str("pub fn main() -> i32 { 0 }");
        let (_, errors) = lang_service.validate(SOURCE);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_validate_syntax_errors() {
        let mut lang_service = new_service_from_str("fn f() { bad!! syntax!! }");
        let (_, errors) = lang_service.validate(SOURCE);
        assert_ne!(errors.len(), 0);
    }

    #[test]
    fn test_validate_name_resolution_errors() {
        let mut lang_service = new_service_from_str("fn f() { g(); }");
        let (_, errors) = lang_service.validate(SOURCE);
        assert_ne!(errors.len(), 0);
    }

    #[test]
    fn test_validate_type_errors() {
        let mut lang_service = new_service_from_str("fn f() { 2_i32 + 3_f64 }");
        let (_, errors) = lang_service.validate(SOURCE);
        assert_ne!(errors.len(), 0);
    }
}
