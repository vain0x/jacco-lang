use crate::{
    clang::clang_dump,
    cps::{eliminate_unit, resolve_types, KEnumOutline},
    front::{cps_conversion, resolve_name, validate_syntax},
    logs::{LogItem, Logs},
    parse::{parse_tokens, PToken, PTokens},
    source::{loc::LocResolver, Doc, TRange},
    token::{tokenize, TokenSource},
};
use log::{error, trace};
use std::{
    io,
    path::{Component, Path, PathBuf},
    process,
    rc::Rc,
};

/// 一連のコンパイル処理を行う。
pub fn compile(source_path: &Path, source_code: &str) -> String {
    trace!("source_path = {:?}", source_path);

    let logs = Logs::new();

    let doc = Doc::new(1);
    let source_path = make_path_relative_to_manifest_dir(source_path);
    Doc::set_path(doc, &source_path);

    let token_source = TokenSource::File(doc);
    let source_code = Rc::new(source_code.to_string());
    let tokens = tokenize(token_source, source_code.clone());
    trace!("tokens = {:#?}\n", tokens);

    let mut p_root = parse_tokens(tokens, logs.logger());
    validate_syntax(&p_root, logs.logger());
    let name_resolution = resolve_name(&mut p_root, logs.logger());
    trace!("p_root = {:#?}\n", p_root);

    if logs.is_fatal() {
        report_logs(
            logs.finish(),
            &MyLocResolver {
                doc_path: &source_path,
                tokens: &p_root.tokens,
            },
        );
        process::exit(1);
    }

    let mut k_root = cps_conversion(&p_root, &name_resolution, logs.logger());
    resolve_types(&mut k_root, logs.logger());
    trace!("k_root (gen) = {:#?}\n", k_root);

    eliminate_unit(&mut k_root);
    trace!("k_root (elim) = {:#?}\n", k_root);

    report_logs(
        logs.finish(),
        &MyLocResolver {
            doc_path: &source_path,
            tokens: &p_root.tokens,
        },
    );

    KEnumOutline::determine_tags(
        &mut k_root.outlines.consts,
        &mut k_root.outlines.enums,
        &mut k_root.outlines.structs,
    );

    clang_dump(&k_root)
}

pub(crate) struct MyLocResolver<'a> {
    doc_path: &'a Path,
    tokens: &'a PTokens,
}

impl<'a> LocResolver for MyLocResolver<'a> {
    fn doc_path(&self, _doc: Doc) -> Option<&Path> {
        Some(self.doc_path)
    }

    fn token_range(&self, _doc: Doc, token: PToken) -> TRange {
        token.location(self.tokens).range().into()
    }
}

fn report_logs(logs: impl IntoIterator<Item = LogItem>, resolver: &impl LocResolver) {
    for item in logs {
        let (message, path_opt, range) = item.resolve(resolver);
        let path = path_opt
            .map(|path| path.to_string_lossy().to_string())
            .unwrap_or_else(String::new);
        error!("{}:{:?} {}", path, range, message);
    }
}

/// ワークスペースのルートからの相対パスを計算する。
fn make_path_relative_to_manifest_dir(path: &Path) -> PathBuf {
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

    make_relative_path(path, &base_dir).unwrap_or_default()
}
