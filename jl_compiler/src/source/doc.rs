use crate::utils::RawId;
use std::fmt::{self, Debug, Display, Formatter};

/// テキストドキュメントの ID
///
/// テキストドキュメント: ファイルやエディターのタブなど、ソースコードの出処となるもの
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Doc {
    raw_id: RawId,
}

impl Doc {
    pub(crate) const MAX: Doc = Doc { raw_id: RawId::MAX };

    pub(crate) const fn new(id: usize) -> Self {
        Self {
            raw_id: RawId::from_index(id),
        }
    }

    pub(crate) fn id(self) -> usize {
        self.raw_id.to_index()
    }

    pub(crate) fn inner(self) -> RawId {
        self.raw_id
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

impl From<usize> for Doc {
    fn from(value: usize) -> Self {
        Self::new(value)
    }
}

mod debug_info {
    use super::Doc;
    use std::{
        path::{Path, PathBuf},
        sync::Arc,
    };

    #[cfg(debug_assertions)]
    use {
        log::error,
        once_cell::sync::Lazy,
        std::{collections::HashMap, sync::Mutex},
    };

    /// For debug.
    #[cfg(debug_assertions)]
    static DOC_PATHS: Lazy<Mutex<HashMap<Doc, Arc<PathBuf>>>> = Lazy::new(Mutex::default);

    impl Doc {
        #[allow(unused)]
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

        #[allow(unused)]
        pub(crate) fn get_path(doc: Doc) -> Option<Arc<PathBuf>> {
            #[cfg(debug_assertions)]
            {
                let map = match DOC_PATHS.try_lock() {
                    Ok(map) => map,
                    Err(err) => {
                        error!("couldn't lock mutex {:?}", err);
                        return None;
                    }
                };

                map.get(&doc).cloned()
            }

            #[cfg(not(debug_assertions))]
            {
                None
            }
        }
    }
}
