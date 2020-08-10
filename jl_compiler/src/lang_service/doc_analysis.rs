// テキストドキュメント単位の解析

use crate::{
    cps::{KMod, KModData, KModOutline},
    front,
    logs::{DocLogs, Logs},
    parse::{self, PTree},
    source::{Doc, TRange},
    token,
};
use std::{path::PathBuf, rc::Rc, sync::Arc};

pub(super) struct Syntax {
    pub(super) tree: PTree,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct Symbols {
    pub(super) mod_outline: KModOutline,
    pub(super) mod_data: KModData,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct DocSymbolAnalysisMut<'a> {
    pub(crate) syntax: &'a mut Syntax,
    pub(crate) symbols: &'a mut Symbols,
}

pub(super) struct AnalysisCache {
    pub(super) doc: Doc,
    pub(super) version: i64,
    pub(super) text: Rc<String>,
    pub(super) source_path: Arc<PathBuf>,
    pub(super) syntax_opt: Option<Syntax>,
    pub(super) symbols_opt: Option<Symbols>,
}

impl AnalysisCache {
    pub(crate) fn version(&self) -> i64 {
        self.version
    }

    pub(super) fn set_text(&mut self, version: i64, text: Rc<String>) {
        self.version = version;
        self.text = text;
        self.syntax_opt = None;
        self.symbols_opt = None;
    }

    pub(super) fn request_syntax(&mut self) -> &mut Syntax {
        if self.syntax_opt.is_some() {
            return self.syntax_opt.as_mut().unwrap();
        }

        let logs = Logs::new();
        let tokens = {
            Doc::set_path(self.doc, &self.source_path);
            let source_code = self.text.clone();
            token::tokenize(source_code)
        };
        let syntax = {
            let tree = parse::parse_tokens(tokens, self.doc, logs.logger());

            let doc_logs = DocLogs::new();
            // FIXME: 構文検査
            // validate_syntax(&root, doc_logs.logger());

            logs.logger().extend_from_doc_logs(self.doc, doc_logs);
            let errors = logs_into_errors(logs, &tree);

            Syntax { tree, errors }
        };

        self.syntax_opt = Some(syntax);
        self.syntax_opt.as_mut().unwrap()
    }

    pub(super) fn request_symbols(&mut self) -> DocSymbolAnalysisMut {
        if self.syntax_opt.is_some() && self.symbols_opt.is_some() {
            return DocSymbolAnalysisMut {
                syntax: self.syntax_opt.as_mut().unwrap(),
                symbols: self.symbols_opt.as_mut().unwrap(),
            };
        }

        let doc = self.doc;
        let k_mod = KMod::from_index(0);
        let symbols = {
            let syntax = self.request_syntax();

            let doc_logs = DocLogs::new();
            let (mod_outline, decl_symbols) =
                front::generate_outline(doc, &syntax.tree, &doc_logs.logger());
            let mod_data = front::convert_to_cps(
                doc,
                k_mod,
                &syntax.tree,
                &decl_symbols,
                &mod_outline,
                &doc_logs.logger(),
            );

            let errors = {
                let logs = Logs::new();
                logs.logger().extend_from_doc_logs(doc, doc_logs);
                logs_into_errors(logs, &syntax.tree)
            };

            Symbols {
                mod_outline,
                mod_data,
                errors,
            }
        };

        self.symbols_opt = Some(symbols);

        DocSymbolAnalysisMut {
            syntax: self.syntax_opt.as_mut().unwrap(),
            symbols: self.symbols_opt.as_mut().unwrap(),
        }
    }
}

fn logs_into_errors(logs: Logs, tree: &PTree) -> Vec<(TRange, String)> {
    logs.finish()
        .into_iter()
        .map(|item| {
            let mut message = item.message().to_string();
            let range = match item.loc().inner().and_then(|(_, loc)| loc.range(tree)) {
                Ok(it) => it,
                Err(hint) => {
                    message += &format!(" loc={}", hint);
                    TRange::ZERO
                }
            };
            (range, message)
        })
        .collect()
}
