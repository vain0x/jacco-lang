// テキストドキュメント単位の解析

use crate::{
    cps::{resolve_types, KMod, KModData, KModOutline, KModOutlines},
    front::{self, validate_syntax, NameResolution, Occurrences},
    logs::{DocLogs, Logs},
    parse::{self, PRoot, PToken},
    source::{loc::LocResolver, Doc, TPos, TRange},
    token::{self, TokenSource},
};
use std::{
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

pub(super) struct Syntax {
    root: PRoot,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct Symbols {
    pub(super) name_resolution_opt: Option<NameResolution>,
    pub(super) occurrences: Occurrences,
    pub(super) errors: Vec<(TRange, String)>,
}

#[allow(unused)]
pub(super) struct Cps {
    pub(super) mod_outline: KModOutline,
    pub(super) root: KModData,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct AnalysisCache {
    pub(super) doc: Doc,
    pub(super) version: i64,
    pub(super) text: Rc<String>,
    pub(super) source_path: Arc<PathBuf>,
    pub(super) syntax_opt: Option<Syntax>,
    pub(super) symbols_opt: Option<Symbols>,
    pub(super) cps_opt: Option<Cps>,
}

impl AnalysisCache {
    pub(crate) fn version(&self) -> i64 {
        self.version
    }
}

impl LocResolver for AnalysisCache {
    fn doc_path(&self, _doc: Doc) -> Option<&Path> {
        Some(self.source_path.as_ref())
    }

    fn token_range(&self, _doc: Doc, token: PToken) -> TRange {
        match &self.syntax_opt {
            Some(syntax) => token.loc(&syntax.root.tokens).range(),
            None => TPos::ZERO.to_empty_range(),
        }
    }
}

impl AnalysisCache {
    pub(super) fn set_text(&mut self, version: i64, text: Rc<String>) {
        self.version = version;
        self.text = text;
        self.syntax_opt = None;
        self.symbols_opt = None;
        self.cps_opt = None;
    }

    pub(super) fn request_syntax(&mut self) -> &mut Syntax {
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

            let doc_logs = DocLogs::new();
            validate_syntax(&root, doc_logs.logger());

            let errors = {
                logs.logger()
                    .extend_from_doc_logs(self.doc, doc_logs, &root);
                logs_into_errors(logs, self)
            };

            Syntax { root, errors }
        };

        self.syntax_opt = Some(syntax);
        self.syntax_opt.as_mut().unwrap()
    }

    pub(super) fn request_symbols(&mut self) -> &mut Symbols {
        if self.symbols_opt.is_some() {
            return self.symbols_opt.as_mut().unwrap();
        }

        let doc = self.doc;
        let symbols = {
            let syntax = self.request_syntax();

            let doc_logs = DocLogs::new();
            let res = front::resolve_name(&mut syntax.root, doc_logs.logger());

            let res = Rc::new(res);
            let occurrences = {
                let res = res.clone();
                front::collect_occurrences(&syntax.root, res)
            };
            let res = Rc::try_unwrap(res).ok().unwrap();

            let errors = {
                let logs = Logs::new();
                logs.logger()
                    .extend_from_doc_logs(doc, doc_logs, &syntax.root);
                logs_into_errors(logs, self)
            };

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
    #[allow(unused)]
    fn request_cps(&mut self) -> &mut Cps {
        if self.cps_opt.is_some() {
            return self.cps_opt.as_mut().unwrap();
        }

        let cps = {
            self.request_symbols();
            let syntax = self.syntax_opt.as_mut().unwrap();
            let symbols = self.symbols_opt.as_mut().unwrap();

            let logs = Logs::new();
            let doc_logs = DocLogs::new();
            let (mut outline, mut root) = front::cps_conversion(
                KMod::TODO,
                &syntax.root,
                symbols.name_resolution_opt.as_ref().unwrap(),
                doc_logs.logger(),
            );

            logs.logger()
                .extend_from_doc_logs(self.doc, doc_logs, &syntax.root);

            // FIXME: mod_outlines を用意する
            resolve_types(
                KMod::from_index(0),
                &outline,
                &mut root,
                &KModOutlines::default(),
                logs.logger(),
            );

            let errors = logs_into_errors(logs, self);

            Cps {
                mod_outline: outline,
                root,
                errors,
            }
        };

        self.cps_opt = Some(cps);
        self.cps_opt.as_mut().unwrap()
    }
}

fn logs_into_errors(logs: Logs, resolver: &impl LocResolver) -> Vec<(TRange, String)> {
    logs.finish()
        .into_iter()
        .map(|item| {
            let (message, _, range) = item.resolve(resolver);
            (range.into(), message.to_string())
        })
        .collect()
}
