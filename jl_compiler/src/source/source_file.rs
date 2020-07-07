use super::Doc;
use fmt::Display;
use log::error;
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fmt::{self, Debug},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SourceFile {
    pub(crate) doc: Doc,
}

/// For debug.
static SOURCE_FILE_PATHS: Lazy<Mutex<HashMap<SourceFile, Arc<PathBuf>>>> =
    Lazy::new(Mutex::default);

impl SourceFile {
    pub(crate) fn set_path(source_file: SourceFile, source_path: &Path) {
        let mut map = match SOURCE_FILE_PATHS.try_lock() {
            Ok(map) => map,
            Err(err) => {
                error!("couldn't lock mutex {:?}", err);
                return;
            }
        };

        map.insert(source_file, source_path.to_path_buf().into());
    }

    pub(crate) fn get_path(source_file: SourceFile) -> Option<Arc<PathBuf>> {
        let map = match SOURCE_FILE_PATHS.try_lock() {
            Ok(map) => map,
            Err(err) => {
                error!("couldn't lock mutex {:?}", err);
                return None;
            }
        };

        map.get(&source_file).cloned()
    }
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match SourceFile::get_path(*self) {
            Some(path) => Display::fmt(&path.to_string_lossy(), f),
            None => Debug::fmt(&self.doc, f),
        }
    }
}
