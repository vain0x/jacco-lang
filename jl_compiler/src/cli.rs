use crate::{
    clang::clang_dump,
    cps::*,
    front::name_resolution::NameSymbols,
    logs::{DocLogs, Logs},
    parse::{parse_tokens, PTree},
    source::{Doc, TRange},
    token::tokenize,
    utils::VecArena,
};
use log::error;
use std::{
    collections::{HashMap, HashSet},
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
    tree: PTree,
    mod_names: Vec<String>,
    logs: DocLogs,
}

type SemanticsArena = VecArena<DocTag, SemanticsData>;

struct SemanticsData {
    #[allow(unused)]
    k_mod: KMod,
    name_symbols: NameSymbols,
}

#[derive(Default)]
pub struct Project {
    docs: DocArena,
    doc_name_map: HashMap<String, Doc>,
    syntaxes: SyntaxArena,
    semantics: SemanticsArena,
    mod_outline: KModOutline,
    mod_data: KModData,
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

    fn logs_into_errors(&mut self, logs: Logs, errors: &mut Vec<(Doc, PathBuf, TRange, String)>) {
        for item in logs.finish() {
            let (doc, loc) = match item.loc().inner() {
                Ok(it) => it,
                Err(hint) => {
                    error!("'{}' {}", hint, item.message());
                    continue;
                }
            };

            let mut message = item.message().to_string();
            let tree = &self.syntaxes[doc.inner()].tree;
            let range = match loc.range(tree) {
                Ok(it) => it,
                Err(hint) => {
                    message += &format!(" loc={}", hint);
                    TRange::ZERO
                }
            };
            let path = self.docs[doc.inner()].path.to_path_buf();
            errors.push((doc, path, range, message));
        }
    }

    /// プロジェクト内の各ドキュメントをパースする。
    ///
    /// unresolved_mod_names に、use 宣言から参照されているドキュメントの名前のうち、
    /// まだパースされていないものが列挙される。
    /// (この呼び出しでパースされたドキュメントはパース済みとみなされるので、
    ///  unresolved_mod_names には含まれない。)
    pub fn parse(&mut self, unresolved_mod_names: &mut Vec<String>) {
        // FIXME: mod_name? doc_name?
        let mut mod_names = vec![];

        let offset = self.syntaxes.len();
        let additional = self.docs.len().saturating_sub(offset);
        self.syntaxes.reserve(additional);

        for (id, doc_data) in self.docs.enumerate().skip(offset) {
            let logs = DocLogs::new();
            let tokens = tokenize(doc_data.text.clone().into());
            let tree = parse_tokens(tokens, logs.logger());
            tree.write_trace();
            tree.collect_used_mod_names(&mut mod_names);

            let id2 = self.syntaxes.alloc(SyntaxData {
                tree,
                mod_names: mod_names.split_off(0),
                logs,
            });
            assert_eq!(id2, id);
        }

        // use 文から参照される不明なモジュール名を列挙して、報告する。
        let mut mod_names = HashSet::new();
        for syntax_data in self.syntaxes.iter() {
            for mod_name in &syntax_data.mod_names {
                if !self.doc_name_map.contains_key(mod_name) {
                    mod_names.insert(mod_name);
                }
            }
        }
        unresolved_mod_names.extend(mod_names.iter().map(ToString::to_string));
    }

    /// プロジェクトをコンパイルして、C言語のソースコードを生成する。
    ///
    /// パースされていないドキュメントは単に無視される。
    pub fn compile_v2(&mut self) -> Result<String, Vec<(Doc, PathBuf, TRange, String)>> {
        let logs = Logs::new();

        self.semantics.reserve(self.syntaxes.len());

        // アウトライン生成
        for (id, syntax) in self.syntaxes.enumerate_mut() {
            let doc = Doc::from(id.to_index());
            let doc_name = self.docs[id].name.to_string();
            let doc_logs = take(&mut syntax.logs);

            let k_mod = self.mod_outline.mods.alloc(KModInfo { name: doc_name });
            let name_symbols = super::front::generate_outline(
                doc,
                &syntax.tree,
                &mut self.mod_outline,
                &doc_logs.logger(),
            );

            syntax.logs = doc_logs;

            let id2 = self.semantics.alloc(SemanticsData {
                k_mod,
                name_symbols,
            });
            assert_eq!(id2, id);
        }

        // エイリアス解決
        let mut aliases = take(&mut self.mod_outline.aliases);
        resolve_aliases(&mut aliases, &self.mod_outline, logs.logger());
        self.mod_outline.aliases = aliases;

        // CPS 変換
        for ((id, syntax), semantics) in
            self.syntaxes.enumerate_mut().zip(self.semantics.iter_mut())
        {
            let doc = Doc::from(id.to_index());
            let doc_logs = take(&mut syntax.logs);

            super::front::convert_to_cps(
                doc,
                &syntax.tree,
                &mut semantics.name_symbols,
                &self.mod_outline,
                &mut self.mod_data,
                &doc_logs.logger(),
            );

            logs.logger().extend_from_doc_logs(doc, doc_logs);
        }

        if logs.is_fatal() {
            let mut errors = vec![];
            self.logs_into_errors(logs, &mut errors);
            return Err(errors);
        }

        let mut mod_data = take(&mut self.mod_data);
        resolve_types(&self.mod_outline, &mut mod_data, logs.logger());
        self.mod_data = mod_data;

        super::cps::eval_cps(&mut self.mod_outline, &mut self.mod_data, &logs.logger());

        if logs.is_fatal() {
            let mut errors = vec![];
            self.logs_into_errors(logs, &mut errors);
            return Err(errors);
        }

        KConstEnumOutline::determine_tags(
            &mut self.mod_outline.consts,
            &self.mod_outline.const_enums,
        );

        eliminate_unit(&mut self.mod_outline, &mut self.mod_data);

        Ok(clang_dump(&self.mod_outline, &self.mod_data))
    }
}

pub struct SyntaxDump {
    pub tree: String,
    pub errors: Vec<(TRange, String)>,
}

/// ファイルをパースして構文木とエラーを得る。(コード生成は行わない。)
pub fn dump_syntax(source_path: &Path, source_code: &str) -> SyntaxDump {
    let mut project = Project::new();

    let name = source_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("<main>")
        .to_string();
    let source_path = make_path_relative_to_manifest_dir(source_path);
    let text = source_code.to_string();
    project.insert(name, source_path, text).ok().unwrap();

    let mut unresolved_doc_names = vec![];
    project.parse(&mut unresolved_doc_names);
    // assert_eq!(
    //     unresolved_doc_names.len(),
    //     0,
    //     "use を含むコードの構文木の取り出しは未実装",
    // );
    debug_assert_eq!(project.syntaxes.len(), 1);

    let mut tree = String::new();
    let mut errors = vec![];
    let logs = Logs::new();
    if let Some((doc, syntax)) = project.syntaxes.enumerate_mut().next() {
        let doc_logs = take(&mut syntax.logs);
        logs.logger()
            .extend_from_doc_logs(Doc::from(doc.to_index()), doc_logs);

        tree = format!("{:#?}", syntax.tree);
    }

    project.logs_into_errors(logs, &mut errors);
    let errors = errors
        .into_iter()
        .map(|(_, _, range, message)| (range, message))
        .collect();

    SyntaxDump { tree, errors }
}

/// 一連のコンパイル処理を行う。
pub fn compile_v2(source_path: &Path, source_code: &str) -> Option<String> {
    let project_dir = source_path.parent();
    let mut project = Project::new();

    let name = source_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("<main>")
        .to_string();
    let source_path = make_path_relative_to_manifest_dir(source_path);
    let text = source_code.to_string();
    project.insert(name, source_path, text).ok().unwrap();

    // 一度探して、見つからなかったファイル名。
    let mut missed_files = HashSet::new();

    let mut unresolved_doc_names = vec![];
    loop {
        assert!(unresolved_doc_names.is_empty());

        project.parse(&mut unresolved_doc_names);

        if unresolved_doc_names.is_empty() {
            break;
        }

        let project_dir = project_dir.unwrap();
        let mut stuck = true;

        // use されているファイル名を列挙する。(このあたりの仕様はよく決まっていない。とりあえず、最初に与えられたファイルと同じディレクトリにある <name>.jacco ファイルを読むことにする。)
        for doc_name in unresolved_doc_names.drain(..) {
            let file_path = project_dir.join(format!("{}.jacco", doc_name));
            if missed_files.contains(&file_path) {
                continue;
            }

            let source_code = match std::fs::read_to_string(&file_path) {
                Ok(text) => text,
                Err(_) => {
                    missed_files.insert(file_path.to_path_buf());
                    continue;
                }
            };

            project
                .insert(doc_name, file_path.to_path_buf(), source_code)
                .ok();
            stuck = false;
        }

        if stuck {
            break;
        }
    }

    if !missed_files.is_empty() {
        for path in missed_files {
            error!(
                "このファイルが use 文で必要とされていますが、見つかりませんでした {:?}",
                path.to_string_lossy(),
            );
        }
        return None;
    }

    match project.compile_v2() {
        Ok(code) => Some(code),
        Err(errors) => {
            if errors.is_empty() {
                unreachable!("エラーがないのにコンパイルエラーが発生しています。");
            }
            for (_, path, range, message) in errors {
                error!("{}:{} {}", path.to_string_lossy(), range, message);
            }
            None
        }
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
