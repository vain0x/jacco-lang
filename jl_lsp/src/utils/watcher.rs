#![allow(unused)]

// 未使用・未実装

use super::uri::Uri;
use fs::OpenOptions;
use jl_compiler::rust_api::Doc;
use notify::{DebouncedEvent, RecommendedWatcher};
use std::{
    collections::{HashMap, HashSet},
    fs,
    io::Read,
    path::{Path, PathBuf},
    rc::Rc,
    sync::mpsc::{Receiver, TryRecvError},
};

/// テキストドキュメントのバージョン番号
/// (エディタ上で編集されるたびに変わる番号。
///  いつの状態のテキストドキュメントを指しているかを明確にするためのもの。)
type TextDocumentVersion = i64;

pub(crate) const NO_VERSION: i64 = 1;

pub(crate) enum DocChange {
    Opened { doc: Doc, text: Rc<String> },
    Changed { doc: Doc, text: Rc<String> },
    Closed { doc: Doc },
}

/// テキストドキュメントを管理するもの。
#[derive(Default)]
pub(crate) struct Watcher {
    last_doc: usize,
    doc_to_uri_map: HashMap<Doc, Uri>,
    uri_to_doc_map: HashMap<Uri, Doc>,
    doc_versions: HashMap<Doc, TextDocumentVersion>,
    file_watcher: Option<RecommendedWatcher>,
    file_event_rx: Option<Receiver<DebouncedEvent>>,
    doc_changes: Vec<DocChange>,
    temp_buf: Vec<u8>,
}

impl Watcher {
    pub(crate) fn new(hsp_root: PathBuf) -> Self {
        Default::default()
    }

    pub(crate) fn fresh_doc(&mut self) -> Doc {
        todo!()
    }

    fn resolve_uri(&mut self, uri: Uri) -> Doc {
        match self.uri_to_doc_map.get(&uri) {
            Some(&doc) => doc,
            None => {
                let doc = self.fresh_doc();
                self.doc_to_uri_map.insert(doc, uri.clone());
                self.uri_to_doc_map.insert(uri, doc);
                doc
            }
        }
    }

    pub(crate) fn find_by_uri(&self, uri: &Uri) -> Option<Doc> {
        self.uri_to_doc_map.get(uri).cloned()
    }

    pub(crate) fn get_uri(&self, doc: Doc) -> Option<&Uri> {
        self.doc_to_uri_map.get(&doc)
    }

    pub(crate) fn get_version(&self, doc: Doc) -> Option<TextDocumentVersion> {
        self.doc_versions.get(&doc).copied()
    }

    pub(crate) fn drain_doc_changes(&mut self, changes: &mut Vec<DocChange>) {
        changes.extend(self.doc_changes.drain(..));
    }

    pub(crate) fn did_initialize(&mut self) {
        self.scan_files();

        if let Some((file_watcher, file_event_rx)) = self.start_file_watcher() {
            self.file_watcher = Some(file_watcher);
            self.file_event_rx = Some(file_event_rx);
        }
    }

    fn scan_files(&mut self) -> Option<()> {
        None
    }

    fn start_file_watcher(&mut self) -> Option<(RecommendedWatcher, Receiver<DebouncedEvent>)> {
        None
    }

    pub(crate) fn poll(&mut self) {}

    fn shutdown_file_watcher(&mut self) {}

    pub(crate) fn shutdown(&mut self) {
        self.shutdown_file_watcher();
    }

    fn do_open_doc(&mut self, uri: Uri, version: i64, text: Rc<String>) -> Doc {
        let doc = self.resolve_uri(uri);

        self.doc_versions.insert(doc, version);
        self.doc_changes.push(DocChange::Opened { doc, text });

        doc
    }

    fn do_change_doc(&mut self, uri: Uri, version: i64, text: Rc<String>) {
        let doc = self.resolve_uri(uri);
        self.doc_versions.insert(doc, version);
        self.doc_changes.push(DocChange::Changed { doc, text });
    }

    fn do_close_doc(&mut self, uri: Uri) {
        if let Some(&doc) = self.uri_to_doc_map.get(&uri) {
            self.doc_to_uri_map.remove(&doc);
            self.doc_versions.remove(&doc);
            self.doc_changes.push(DocChange::Closed { doc })
        }

        self.uri_to_doc_map.remove(&uri);
    }

    pub(crate) fn open_doc(&mut self, uri: Uri, version: i64, text: String) {}

    pub(crate) fn change_doc(&mut self, uri: Uri, version: i64, text: String) {}

    pub(crate) fn close_doc(&mut self, uri: Uri) {}

    pub(crate) fn change_file(&mut self, path: &Path) -> Option<()> {
        None
    }

    pub(crate) fn close_file(&mut self, path: &Path) -> Option<()> {
        None
    }
}

fn file_ext_is_watched(path: &Path) -> bool {
    path.extension().map_or(false, |ext| ext == "jacco")
}

fn read_file(path: &Path, out: &mut Vec<u8>) -> bool {
    let mut file = match OpenOptions::new().read(true).open(path) {
        Err(_) => return false,
        Ok(file) => file,
    };

    file.read_to_end(out).is_ok()
}
