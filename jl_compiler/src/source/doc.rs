use std::fmt::{self, Debug, Display, Formatter};

/// テキストドキュメントの ID
///
/// テキストドキュメント: ファイルやエディターのタブなど、ソースコードの出処となるもの
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct Doc {
    id: u32,
}

impl Doc {
    pub(crate) fn new(id: usize) -> Self {
        Self { id: id as u32 }
    }

    pub(crate) fn id(self) -> usize {
        self.id as usize
    }
}

impl Debug for Doc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match Doc::get_path(*self) {
            Some(path) => Display::fmt(&path.to_string_lossy(), f),
            None => write!(f, "doc#{}", self.id()),
        }
    }
}

mod debug_info {
    use super::Doc;
    use log::error;
    use once_cell::sync::Lazy;
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
        sync::{Arc, Mutex},
    };

    /// For debug.
    static DOC_PATHS: Lazy<Mutex<HashMap<Doc, Arc<PathBuf>>>> = Lazy::new(Mutex::default);

    impl Doc {
        pub(crate) fn set_path(doc: Doc, source_path: &Path) {
            #[cfg(debug_assertions)]
            {
                let mut map = match DOC_PATHS.try_lock() {
                    Ok(map) => map,
                    Err(err) => {
                        error!("couldn't lock mutex {:?}", err);
                        return;
                    }
                };

                map.insert(doc, source_path.to_path_buf().into());
            }
        }

        pub(crate) fn get_path(doc: Doc) -> Option<Arc<PathBuf>> {
            let map = match DOC_PATHS.try_lock() {
                Ok(map) => map,
                Err(err) => {
                    error!("couldn't lock mutex {:?}", err);
                    return None;
                }
            };

            map.get(&doc).cloned()
        }
    }
}
