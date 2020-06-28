use super::uri::Uri;
use fs::OpenOptions;
use jl_compiler::rust_api::Source;
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
    Opened { source: Source, text: Rc<String> },
    Changed { source: Source, text: Rc<String> },
    Closed { source: Source },
}

/// テキストドキュメントを管理するもの。
#[derive(Default)]
pub(crate) struct Watcher {
    last_source: usize,
    source_to_uri: HashMap<Source, Uri>,
    uri_to_source: HashMap<Uri, Source>,
    source_versions: HashMap<Source, TextDocumentVersion>,
    file_watcher: Option<RecommendedWatcher>,
    file_event_rx: Option<Receiver<DebouncedEvent>>,
    source_changes: Vec<DocChange>,
    temp_buf: Vec<u8>,
}

impl Watcher {
    pub(crate) fn new(hsp_root: PathBuf) -> Self {
        Default::default()
    }

    pub(crate) fn fresh_source(&mut self) -> Source {
        todo!()
    }

    fn resolve_uri(&mut self, uri: Uri) -> Source {
        match self.uri_to_source.get(&uri) {
            Some(&source) => source,
            None => {
                let source = self.fresh_source();
                self.source_to_uri.insert(source, uri.clone());
                self.uri_to_source.insert(uri, source);
                source
            }
        }
    }

    pub(crate) fn find_by_uri(&self, uri: &Uri) -> Option<Source> {
        self.uri_to_source.get(uri).cloned()
    }

    pub(crate) fn get_uri(&self, source: Source) -> Option<&Uri> {
        self.source_to_uri.get(&source)
    }

    pub(crate) fn get_version(&self, source: Source) -> Option<TextDocumentVersion> {
        self.source_versions.get(&source).copied()
    }

    pub(crate) fn drain_doc_changes(&mut self, changes: &mut Vec<DocChange>) {
        changes.extend(self.source_changes.drain(..));
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

    fn do_open_doc(&mut self, uri: Uri, version: i64, text: Rc<String>) -> Source {
        let source = self.resolve_uri(uri);

        self.source_versions.insert(source, version);
        self.source_changes.push(DocChange::Opened { source, text });

        source
    }

    fn do_change_doc(&mut self, uri: Uri, version: i64, text: Rc<String>) {
        let source = self.resolve_uri(uri);
        self.source_versions.insert(source, version);
        self.source_changes
            .push(DocChange::Changed { source, text });
    }

    fn do_close_doc(&mut self, uri: Uri) {
        if let Some(&source) = self.uri_to_source.get(&uri) {
            self.source_to_uri.remove(&source);
            self.source_versions.remove(&source);
            self.source_changes.push(DocChange::Closed { source })
        }

        self.uri_to_source.remove(&uri);
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
