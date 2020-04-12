use std::fmt;
use std::io;
use std::path::{Component, Path, PathBuf};
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct SourceFile {
    pub(crate) source_path: Rc<PathBuf>,
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn segments(path: &Path) -> io::Result<Vec<String>> {
            Ok(path
                .canonicalize()?
                .components()
                .filter_map(|c| match c {
                    Component::Normal(name) => Some(name.to_string_lossy().to_string()),
                    _ => None,
                })
                .collect())
        }

        fn make_relative_path(dest_path: &Path, base_path: &Path) -> io::Result<PathBuf> {
            let dest = segments(dest_path)?;
            let base = segments(base_path)?;

            let common_prefix_len = dest
                .iter()
                .zip(base.iter())
                .take_while(|(dest_name, base_name)| dest_name == base_name)
                .count();

            let mut out = PathBuf::new();
            for _ in 0..base.len() - common_prefix_len {
                out.push("..".to_string());
            }
            for name in &dest[common_prefix_len..] {
                out.push(name);
            }
            Ok(out)
        }

        let manifest_dir: &str = env!("CARGO_MANIFEST_DIR");

        let mut base_dir = PathBuf::from(manifest_dir);
        base_dir.pop();

        let short_path = make_relative_path(&self.source_path, &base_dir).unwrap();
        write!(f, "{}", short_path.to_string_lossy())
    }
}

impl fmt::Display for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.source_path.to_string_lossy())
    }
}
