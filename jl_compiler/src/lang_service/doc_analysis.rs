// テキストドキュメント単位の解析

use crate::{
    cps::*,
    front::{self, name_resolution::*},
    logs::{DocLogs, Logs},
    parse::{self, PLoc, PTree},
    scope::lexical_referent::LexicalReferent,
    source::{Doc, TRange},
    token,
};
use std::{path::PathBuf, rc::Rc, sync::Arc};

pub(super) type TyUseSites = Vec<(KTy, PLoc)>;

pub(super) struct Syntax {
    pub(super) tree: PTree,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct Symbols {
    pub(super) symbol_count: usize,
    pub(super) name_symbols: NameSymbols,
    pub(super) ty_use_sites: TyUseSites,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(crate) struct Cps {
    pub(super) ty_use_sites: TyUseSites,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct DocSymbolAnalysisMut<'a> {
    pub(crate) syntax: &'a mut Syntax,
    pub(crate) symbols: &'a mut Symbols,
}

pub(super) struct DocContentAnalysisMut<'a> {
    pub(crate) syntax: &'a Syntax,
    pub(crate) symbols: &'a Symbols,
    pub(crate) cps: &'a mut Cps,
    pub(crate) mod_outline: &'a KModOutline,
    pub(crate) mod_data: &'a mut KModData,
}

pub(super) struct AnalysisCache {
    pub(super) doc: Doc,
    pub(super) version: i64,
    pub(super) text: Rc<String>,
    pub(super) source_path: Arc<PathBuf>,
    pub(super) syntax_opt: Option<Syntax>,
    pub(super) symbols_opt: Option<Symbols>,
    pub(super) cps_opt: Option<Cps>,
    pub(super) lost_symbol_count: usize,
}

impl AnalysisCache {
    pub(super) fn new(doc: Doc, version: i64, text: Rc<String>, source_path: Arc<PathBuf>) -> Self {
        Self {
            doc,
            version,
            text,
            source_path,
            syntax_opt: Default::default(),
            symbols_opt: Default::default(),
            cps_opt: Default::default(),
            lost_symbol_count: 0,
        }
    }

    pub(crate) fn version(&self) -> i64 {
        self.version
    }

    pub(super) fn purge_cache(&mut self) {
        self.syntax_opt = None;
        let symbols_opt = self.symbols_opt.take();

        if let Some(symbols) = symbols_opt {
            self.lost_symbol_count += symbols.symbol_count;
        }

        self.cps_opt = None;
    }

    pub(super) fn set_text(&mut self, version: i64, text: Rc<String>) {
        self.version = version;
        self.text = text;
        self.purge_cache();
    }

    pub(super) fn request_syntax(&mut self) -> &mut Syntax {
        if self.syntax_opt.is_some() {
            return self.syntax_opt.as_mut().unwrap();
        }

        let tokens = {
            Doc::set_path(self.doc, &self.source_path);
            let source_code = self.text.clone();
            token::tokenize(source_code)
        };
        let syntax = {
            let doc_logs = DocLogs::new();
            let logs = Logs::new();

            let tree = parse::parse_tokens(tokens, doc_logs.logger());

            logs.logger().extend_from_doc_logs(self.doc, doc_logs);
            let errors = logs_into_errors(logs, &tree);

            Syntax { tree, errors }
        };

        self.syntax_opt = Some(syntax);
        self.syntax_opt.as_mut().unwrap()
    }

    pub(super) fn request_symbols(
        &mut self,
        mod_outline: &mut KModOutline,
    ) -> DocSymbolAnalysisMut {
        if self.syntax_opt.is_some() && self.symbols_opt.is_some() {
            return DocSymbolAnalysisMut {
                syntax: self.syntax_opt.as_mut().unwrap(),
                symbols: self.symbols_opt.as_mut().unwrap(),
            };
        }

        let doc = self.doc;

        let symbols =
            {
                let syntax = self.request_syntax();

                let doc_logs = DocLogs::new();
                let base_symbol_count = mod_outline.symbol_count();
                let name_symbols =
                    front::generate_outline(doc, &syntax.tree, mod_outline, &doc_logs.logger());
                let symbol_count = mod_outline.symbol_count() - base_symbol_count;

                let errors = {
                    let logs = Logs::new();
                    logs.logger().extend_from_doc_logs(doc, doc_logs);
                    logs_into_errors(logs, &syntax.tree)
                };

                let ty_use_sites =
                    {
                        let def_names = name_symbols.keys().map(|&name| (name, name.loc()));
                        let use_names = syntax.tree.name_referents.iter().filter_map(
                            |(&use_name, referent)| match *referent {
                                LexicalReferent::Name(def_name) => Some((def_name, use_name.loc())),
                                _ => None,
                            },
                        );
                        def_names
                            .chain(use_names)
                            .filter_map(|(def_name, loc)| {
                                let ty = name_symbols.get(&def_name)?.as_ty()?;
                                Some((ty, loc))
                            })
                            .collect()
                    };

                // let name_symbols_orig = name_symbols.clone();
                Symbols {
                    symbol_count,
                    name_symbols,
                    ty_use_sites,
                    errors,
                }
            };

        let old = self.symbols_opt.replace(symbols);
        assert!(old.is_none());

        DocSymbolAnalysisMut {
            syntax: self.syntax_opt.as_mut().unwrap(),
            symbols: self.symbols_opt.as_mut().unwrap(),
        }
    }

    pub(super) fn doc_content_analysis_mut<'a>(
        &'a mut self,
        mod_outline: &'a KModOutline,
        mod_data: &'a mut KModData,
    ) -> Option<DocContentAnalysisMut<'a>> {
        Some(DocContentAnalysisMut {
            syntax: self.syntax_opt.as_ref()?,
            symbols: self.symbols_opt.as_ref()?,
            cps: self.cps_opt.as_mut()?,
            mod_outline,
            mod_data,
        })
    }

    pub(super) fn request_cps<'a>(
        &'a mut self,
        mod_outline: &'a mut KModOutline,
        mod_data: &'a mut KModData,
    ) -> DocContentAnalysisMut<'a> {
        if self.syntax_opt.is_some() && self.symbols_opt.is_some() && self.cps_opt.is_some() {
            return self
                .doc_content_analysis_mut(mod_outline, mod_data)
                .unwrap();
        }

        let doc = self.doc;
        let DocSymbolAnalysisMut { syntax, symbols } = self.request_symbols(mod_outline);

        let doc_logs = DocLogs::new();
        let logs = Logs::new();

        front::convert_to_cps(
            doc,
            &syntax.tree,
            &mut symbols.name_symbols,
            mod_outline,
            mod_data,
            &doc_logs.logger(),
        );
        let errors = {
            logs.logger().extend_from_doc_logs(doc, doc_logs);
            logs_into_errors(logs, &syntax.tree)
        };

        self.cps_opt = Some(Cps {
            ty_use_sites: vec![],
            errors,
        });

        self.doc_content_analysis_mut(mod_outline, mod_data)
            .unwrap()
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
