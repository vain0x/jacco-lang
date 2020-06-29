use crate::utils::Uri;
use jl_compiler::rust_api::{LangService, Source};
use std::{collections::HashMap, path::PathBuf, rc::Rc};
use url::Url;

#[derive(Debug)]
pub(crate) enum SourceChange {
    DidOpen {
        source: Source,
        version: i64,
        text: Rc<String>,
        path: Rc<PathBuf>,
    },
    DidChange {
        source: Source,
        version: i64,
        text: Rc<String>,
    },
    DidClose {
        source: Source,
    },
}

struct SourceData {
    uri: Uri,
}

#[derive(Default)]
pub(crate) struct Sources {
    last_id: usize,
    sources: HashMap<Source, SourceData>,
    uri_to_source_map: HashMap<Uri, Source>,
    changes: Vec<SourceChange>,
}

impl Sources {
    pub(crate) fn new() -> Self {
        Sources::default()
    }

    pub(crate) fn drain_changes(&mut self, changes: &mut Vec<SourceChange>) {
        changes.extend(self.changes.drain(..));
    }

    pub(crate) fn source_to_url(&self, source: Source) -> Option<Url> {
        self.sources
            .get(&source)
            .map(|data| data.uri.clone().into_url())
    }

    pub(crate) fn url_to_source(&self, url: &Url) -> Option<Source> {
        let uri = Uri::from_url(&url);
        self.uri_to_source_map.get(&uri).copied()
    }

    pub(crate) fn doc_did_open(&mut self, url: Url, version: i64, text: Rc<String>) {
        let uri = Uri::from_url(&url);

        let source = {
            self.last_id += 1;
            Source::from(self.last_id)
        };
        self.sources.insert(source, SourceData { uri: uri.clone() });
        self.uri_to_source_map.insert(uri.clone(), source);

        let path = {
            let url = uri.into_url();
            let path = url
                .to_file_path()
                .ok()
                .or_else(|| {
                    url.path_segments()
                        .and_then(|segments| segments.last().map(|last| last.to_string()))
                        .map(PathBuf::from)
                })
                .unwrap_or_else(|| PathBuf::from("_.jacco"));
            Rc::new(path)
        };

        self.changes.push(SourceChange::DidOpen {
            source,
            version,
            text,
            path,
        });
    }

    pub(crate) fn doc_did_change(&mut self, url: Url, version: i64, text: Rc<String>) {
        let uri = Uri::from_url(&url);

        let source = match self.uri_to_source_map.get(&uri) {
            Some(&source) => source,
            None => return self.doc_did_open(url, version, text),
        };

        self.changes.push(SourceChange::DidChange {
            source,
            version,
            text,
        });
    }

    pub(crate) fn doc_did_close(&mut self, url: Url) {
        let uri = Uri::from_url(&url);

        let source = match self.uri_to_source_map.get(&uri) {
            Some(&source) => source,
            None => return,
        };

        self.uri_to_source_map.remove(&uri);
        self.sources.remove(&source);

        self.changes.push(SourceChange::DidClose { source });
    }
}
