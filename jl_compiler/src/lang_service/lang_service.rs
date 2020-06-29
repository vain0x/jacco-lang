use crate::{
    front::validate_syntax,
    logs::Logs,
    parse::{self, PRoot},
    source::{Pos, Range, SourceFile},
    token::{self, TokenSource},
};
use log::{error, trace};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
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

#[derive(Default)]
struct AnalysisCache {
    version: i64,
    text: Rc<String>,
    source_path: Rc<PathBuf>,
    syntax_opt: Option<Syntax>,
}

impl AnalysisCache {
    fn set_text(&mut self, version: i64, text: Rc<String>) {
        self.version = version;
        self.text = text;
        self.syntax_opt = None;
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

    pub fn open_doc(&mut self, source: Source, version: i64, text: Rc<String>) {
        self.sources.insert(
            source,
            AnalysisCache {
                version,
                text,
                // FIXME: 引数でもらう
                source_path: Rc::new(PathBuf::from("main.jacco")),
                syntax_opt: None,
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

    pub fn document_highlight(&mut self, source: Source, pos: Pos) -> Vec<()> {
        vec![]
    }

    pub fn hover(&mut self, source: Source, pos: Pos) -> Option<()> {
        None
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
                let errors = analysis.request_syntax().errors.clone();
                let version_opt = Some(analysis.version);
                (version_opt, errors)
            })
            .unwrap_or((None, vec![]))
    }
}
