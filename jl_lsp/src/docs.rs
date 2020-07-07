use crate::utils::Uri;
use jl_compiler::rust_api::{Doc, LangService};
use std::{collections::HashMap, path::PathBuf, rc::Rc};
use url::Url;

#[derive(Debug)]
pub(crate) enum DocChange {
    DidOpen {
        doc: Doc,
        version: i64,
        text: Rc<String>,
        path: Rc<PathBuf>,
    },
    DidChange {
        doc: Doc,
        version: i64,
        text: Rc<String>,
    },
    DidClose {
        doc: Doc,
    },
}

struct DocData {
    uri: Uri,
}

#[derive(Default)]
pub(crate) struct Docs {
    last_id: usize,
    docs: HashMap<Doc, DocData>,
    uri_to_doc_map: HashMap<Uri, Doc>,
    changes: Vec<DocChange>,
}

impl Docs {
    pub(crate) fn new() -> Self {
        Docs::default()
    }

    pub(crate) fn drain_changes(&mut self, changes: &mut Vec<DocChange>) {
        changes.extend(self.changes.drain(..));
    }

    pub(crate) fn doc_to_url(&self, doc: Doc) -> Option<Url> {
        self.docs.get(&doc).map(|data| data.uri.clone().into_url())
    }

    pub(crate) fn url_to_doc(&self, url: &Url) -> Option<Doc> {
        let uri = Uri::from_url(&url);
        self.uri_to_doc_map.get(&uri).copied()
    }

    pub(crate) fn doc_did_open(&mut self, url: Url, version: i64, text: Rc<String>) {
        let uri = Uri::from_url(&url);

        let doc = {
            self.last_id += 1;
            Doc::from(self.last_id)
        };
        self.docs.insert(doc, DocData { uri: uri.clone() });
        self.uri_to_doc_map.insert(uri.clone(), doc);

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

        self.changes.push(DocChange::DidOpen {
            doc,
            version,
            text,
            path,
        });
    }

    pub(crate) fn doc_did_change(&mut self, url: Url, version: i64, text: Rc<String>) {
        let uri = Uri::from_url(&url);

        let doc = match self.uri_to_doc_map.get(&uri) {
            Some(&doc) => doc,
            None => return self.doc_did_open(url, version, text),
        };

        self.changes
            .push(DocChange::DidChange { doc, version, text });
    }

    pub(crate) fn doc_did_close(&mut self, url: Url) {
        let uri = Uri::from_url(&url);

        let doc = match self.uri_to_doc_map.get(&uri) {
            Some(&doc) => doc,
            None => return,
        };

        self.uri_to_doc_map.remove(&uri);
        self.docs.remove(&doc);

        self.changes.push(DocChange::DidClose { doc });
    }
}
