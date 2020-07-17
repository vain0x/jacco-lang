use crate::{
    clang::clang_dump,
    cps::{
        eliminate_unit, resolve_aliases, resolve_types, KEnumOutline, KModData, KModOutline,
        KModTag,
    },
    front::{cps_conversion, resolve_name, validate_syntax, NameResolution},
    logs::Logs,
    parse::{parse_tokens, PRoot, PToken, PTokens},
    source::{loc::LocResolver, Doc, TRange},
    token::{tokenize, TokenSource},
    utils::VecArena,
};
use log::{error, trace};
use std::{
    collections::HashMap,
    io,
    mem::take,
    path::{Component, Path, PathBuf},
};

pub(crate) type DocTag = ();

pub(crate) type DocArena = VecArena<DocTag, DocData>;

pub(crate) struct DocData {
    name: String,
    path: PathBuf,
    text: String,
}

type SyntaxArena = VecArena<DocTag, SyntaxData>;

struct SyntaxData {
    root: PRoot,
    logs: Logs,
}

#[derive(Default)]
pub struct Project {
    docs: DocArena,
    doc_name_map: HashMap<String, Doc>,
    syntaxes: SyntaxArena,
    names: VecArena<DocTag, (NameResolution, Logs)>,
    mod_docs: VecArena<KModTag, Doc>,
    mod_outlines: VecArena<KModTag, KModOutline>,
    mods: VecArena<KModTag, KModData>,
}

impl Project {
    pub fn new() -> Self {
        Project::default()
    }

    /// プロジェクトにドキュメントを追加する。同名のドキュメントを複数追加することはできない。
    pub fn insert(&mut self, name: String, path: PathBuf, text: String) -> Result<Doc, (Doc, Doc)> {
        let id = self.docs.alloc(DocData {
            name: name.clone(),
            path,
            text,
        });
        let doc = Doc::from(id.to_index());

        let duplicated_doc_opt = self.doc_name_map.insert(name, doc);
        if let Some(other) = duplicated_doc_opt {
            return Err((doc, other));
        }
        Ok(doc)
    }

    /// まだパースされていないドキュメントをパースして、use 宣言を検出し、どのドキュメントを参照しているかを報告する。
    pub fn parse(&mut self) {
        self.syntaxes = VecArena::from_iter(self.docs.enumerate().map(|(id, doc_data)| {
            let doc = Doc::from(id.to_index());

            let logs = Logs::new();
            let tokens = {
                let source = TokenSource::File(doc);
                tokenize(source, doc_data.text.clone().into())
            };
            let root = parse_tokens(tokens, logs.logger());

            // FIXME: use 宣言をチェックして、パースされていないドキュメントへの参照があるなら報告する。クライアントは報告された名前のドキュメントをファイルシステムなどから探して insert し、再び parse を呼ぶ。
            // 参照先のドキュメントがすべてパース済みであるドキュメントは名前解決の工程に入れる。

            SyntaxData { root, logs }
        }));
    }

    pub fn compile(
        &mut self,
    ) -> Result<Vec<(Doc, &str, String)>, Vec<(Doc, &Path, TRange, String)>> {
        let mut logs_list = vec![];

        self.names = VecArena::from_iter(self.syntaxes.enumerate_mut().map(|(id, syntax)| {
            let doc = Doc::from(id.to_index());
            let logs = take(&mut syntax.logs);

            let errors1 = validate_syntax(&syntax.root);
            let (name_resolution, errors2) = resolve_name(&mut syntax.root);

            {
                let logger = logs.logger();
                for (loc, message) in errors1.into_iter().chain(errors2) {
                    let range = loc.resolve(&syntax.root);
                    logger.error((doc, range), message);
                }
            }

            (name_resolution, logs)
        }));

        for ((id, pair), syntax) in self.names.enumerate_mut().zip(self.syntaxes.iter()) {
            let doc = Doc::from(id.to_index());

            let name_resolution = &pair.0;
            let logs = take(&mut pair.1);
            if logs.is_fatal() {
                logs_list.push((doc, logs.finish()));
                continue;
            }

            let (mod_outline, mod_data) =
                cps_conversion(&syntax.root, name_resolution, logs.logger());

            let k_mod = self.mod_outlines.alloc(mod_outline);
            {
                let k_mod2 = self.mods.alloc(mod_data);
                assert_eq!(k_mod, k_mod2);
            }
            {
                let k_mod3 = self.mod_docs.alloc(doc);
                assert_eq!(k_mod, k_mod3);
            }
        }

        if !logs_list.is_empty() {
            let mut errors = vec![];
            for (doc, items) in logs_list {
                let doc_data = &self.docs[doc.inner()];
                for item in items {
                    let range = TRange::from(item.location().range());
                    let message = item.message().to_string();
                    errors.push((doc, doc_data.path.as_path(), range, message));
                }
            }
            return Err(errors);
        }

        let logs = Logs::new();
        let mod_ids = self.mod_outlines.keys().collect::<Vec<_>>();
        for k_mod in mod_ids {
            let mut aliases = take(&mut k_mod.of_mut(&mut self.mod_outlines).aliases);
            resolve_aliases(&mut aliases, &self.mod_outlines, logs.logger());
            k_mod.of_mut(&mut self.mod_outlines).aliases = aliases;
        }

        let mut mods = take(&mut self.mods);
        for ((_, mod_outline), ref mut mod_data) in
            self.mod_outlines.enumerate().zip(mods.iter_mut())
        {
            resolve_types(mod_outline, *mod_data, logs.logger());
        }
        self.mods = mods;

        if logs.is_fatal() {
            let mut errors = vec![];
            for item in logs.finish() {
                let doc = match item.location().source {
                    TokenSource::File(doc) => doc,
                    TokenSource::Special(name) => {
                        error!("'{}' {}", name, item.message());
                        continue;
                    }
                };

                let path = self.docs[doc.inner()].path.as_path();
                let range = TRange::from(item.location().range());
                let message = item.message().to_string();
                errors.push((doc, path, range, message));
            }
            return Err(errors);
        }

        for (mod_outline, mod_data) in self.mod_outlines.iter_mut().zip(self.mods.iter_mut()) {
            KEnumOutline::determine_tags(
                &mut mod_outline.consts,
                &mut mod_outline.enums,
                &mut mod_outline.structs,
            );

            eliminate_unit(mod_outline, mod_data);
        }

        let mut c_modules = vec![];
        for (((k_mod, doc), mod_outline), mod_data) in self
            .mod_docs
            .enumerate()
            .zip(self.mod_outlines.iter())
            .zip(self.mods.iter())
        {
            let id = doc.inner();
            let name = &self.docs[id].name;
            let code = clang_dump(k_mod, mod_outline, mod_data, &self.mod_outlines);
            c_modules.push((*doc, name.as_str(), code));
        }
        Ok(c_modules)
    }
}

/// 一連のコンパイル処理を行う。
pub fn compile(source_path: &Path, source_code: &str) -> String {
    trace!("source_path = {:?}", source_path);

    let mut project = Project::new();

    let name = source_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("<main>")
        .to_string();
    let source_path = make_path_relative_to_manifest_dir(source_path);
    let text = source_code.to_string();
    project.insert(name, source_path, text).ok().unwrap();

    project.parse();
    match project.compile() {
        Ok(c_modules) => c_modules
            .into_iter()
            .map(|(_, _, code)| code)
            .collect::<Vec<_>>()
            .join("\n"),
        Err(errors) => {
            for (_, path, range, message) in errors {
                error!("{}:{} {}", path.to_string_lossy(), range, message);
            }
            String::new()
        }
    }
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
