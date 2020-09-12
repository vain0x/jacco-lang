use crate::{
    clang::clang_dump,
    cps::*,
    front::NullNameResolutionListener,
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

#[derive(Default)]
pub struct Project {
    docs: DocArena,
    doc_name_map: HashMap<String, Doc>,
    syntaxes: SyntaxArena,
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

        // use 宣言から参照される不明なモジュール名を列挙して、報告する。
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

        let mut listener = NullNameResolutionListener;
        let mut decl_symbolss = vec![];
        let mut name_symbols_vec = vec![];

        // アウトライン生成
        for (id, syntax) in self.syntaxes.enumerate_mut() {
            let doc = Doc::from(id.to_index());
            let doc_name = self.docs[id].name.to_string();
            let doc_logs = take(&mut syntax.logs);

            let k_mod = self.mod_docs.alloc(doc);
            let (mut mod_outline, decl_symbols, name_symbols) = super::front::generate_outline(
                doc,
                &syntax.tree,
                &mut listener,
                &doc_logs.logger(),
            );
            mod_outline.name = doc_name;
            let k_mod2 = self.mod_outlines.alloc(mod_outline);
            assert_eq!(k_mod, k_mod2);

            syntax.logs = doc_logs;
            decl_symbolss.push(decl_symbols);
            name_symbols_vec.push(name_symbols);
        }

        // エイリアス解決
        let mod_ids = self.mod_outlines.keys().collect::<Vec<_>>();
        for &k_mod in &mod_ids {
            let mut aliases = take(&mut k_mod.of_mut(&mut self.mod_outlines).aliases);
            resolve_aliases(&mut aliases, &self.mod_outlines, logs.logger());
            k_mod.of_mut(&mut self.mod_outlines).aliases = aliases;
        }

        // CPS 変換
        for ((i, k_mod), syntax) in mod_ids
            .iter()
            .copied()
            .enumerate()
            .zip(self.syntaxes.iter_mut())
        {
            let doc = self.mod_docs[k_mod];
            let doc_logs = take(&mut syntax.logs);

            let mod_data = super::front::convert_to_cps(
                doc,
                k_mod,
                &syntax.tree,
                &name_symbols_vec[i],
                &decl_symbolss[i],
                k_mod.of(&self.mod_outlines),
                &self.mod_outlines,
                &mut listener,
                &doc_logs.logger(),
            );
            let k_mod3 = self.mods.alloc(mod_data);
            assert_eq!(k_mod, k_mod3);

            logs.logger().extend_from_doc_logs(doc, doc_logs);
        }

        if logs.is_fatal() {
            let mut errors = vec![];
            self.logs_into_errors(logs, &mut errors);
            return Err(errors);
        }

        let mut mods = take(&mut self.mods);
        for ((k_mod, mod_outline), ref mut mod_data) in
            self.mod_outlines.enumerate().zip(mods.iter_mut())
        {
            resolve_types(
                k_mod,
                mod_outline,
                *mod_data,
                &self.mod_outlines,
                logs.logger(),
            );
        }
        self.mods = mods;

        super::cps::eval_cps(&mut self.mod_outlines, &mut self.mods, &logs.logger());

        if logs.is_fatal() {
            let mut errors = vec![];
            self.logs_into_errors(logs, &mut errors);
            return Err(errors);
        }

        for (mod_outline, mod_data) in self.mod_outlines.iter_mut().zip(self.mods.iter_mut()) {
            KConstEnumOutline::determine_tags(&mut mod_outline.consts, &mod_outline.const_enums);

            eliminate_unit(mod_outline, mod_data);
        }

        Ok(clang_dump(&self.mod_outlines, &self.mods))
    }
}

/// ファイルをパースして構文木とエラーを得る。(コード生成は行わない。)
#[allow(unused)]
pub(crate) fn parse_v2(source_path: &Path, source_code: &str) -> String {
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
    assert_eq!(
        unresolved_doc_names.len(),
        0,
        "use を含むコードの構文木の取り出しは未実装",
    );

    let mut output = String::new();
    let mut errors = vec![];
    let logs = Logs::new();
    for (doc, syntax) in project.syntaxes.enumerate_mut() {
        let doc_logs = take(&mut syntax.logs);
        logs.logger()
            .extend_from_doc_logs(Doc::from(doc.to_index()), doc_logs);

        output = format!("{:#?}", syntax.tree);
    }
    project.logs_into_errors(logs, &mut errors);

    if !errors.is_empty() {
        let error = errors
            .into_iter()
            .map(|(_, _, range, message)| format!("ERROR {:?} {}\n", range, message))
            .collect::<String>();
        output.insert_str(0, &error);
    }

    output
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
                "このファイルが use 宣言で必要とされていますが、見つかりませんでした {:?}",
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
