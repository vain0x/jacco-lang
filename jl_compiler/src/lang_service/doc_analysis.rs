// テキストドキュメント単位の解析

use crate::{
    cps::*,
    front,
    logs::{DocLogs, Logs},
    parse::{self, PTree},
    source::{Doc, TRange},
    token,
    utils::VecArena,
};
use front::NameResolutionListener;
use parse::{ADeclTag, PLoc};
use std::{mem::take, path::PathBuf, rc::Rc, sync::Arc};

pub(super) type TyUseSites = Vec<(KTy, PLoc)>;

pub(super) struct Syntax {
    pub(super) tree: PTree,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct Symbols {
    pub(super) mod_outline: KModOutline,
    pub(super) decl_symbols: VecArena<ADeclTag, Option<KModLocalSymbol>>,
    pub(super) ty_use_sites: TyUseSites,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(crate) struct Cps {
    pub(super) mod_data: KModData,
    pub(super) ty_use_sites: TyUseSites,
    pub(super) errors: Vec<(TRange, String)>,
}

pub(super) struct DocSymbolAnalysisMut<'a> {
    pub(crate) syntax: &'a mut Syntax,
    pub(crate) symbols: &'a mut Symbols,
}

pub(super) struct DocContentAnalysisMut<'a> {
    pub(crate) syntax: &'a Syntax,
    pub(crate) symbols: &'a mut Symbols,
    pub(crate) cps: &'a mut Cps,
}

pub(super) struct AnalysisCache {
    pub(super) doc: Doc,
    pub(super) version: i64,
    pub(super) text: Rc<String>,
    pub(super) source_path: Arc<PathBuf>,
    pub(super) syntax_opt: Option<Syntax>,
    pub(super) symbols_opt: Option<Symbols>,
    pub(super) cps_opt: Option<Cps>,
    pub(super) type_resolution_is_done: bool,
    pub(super) mod_opt: Option<KMod>,
}

#[derive(Default)]
struct ImplNameResolutionListener {
    ty_use_sites: TyUseSites,
}

impl NameResolutionListener for ImplNameResolutionListener {
    fn ty_did_resolve(&mut self, loc: PLoc, ty: &KTy) {
        self.ty_use_sites.push((ty.clone(), loc));
    }
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
            type_resolution_is_done: Default::default(),
            mod_opt: Default::default(),
        }
    }

    pub(crate) fn version(&self) -> i64 {
        self.version
    }

    pub(super) fn set_text(&mut self, version: i64, text: Rc<String>) {
        self.version = version;
        self.text = text;
        self.syntax_opt = None;
        self.symbols_opt = None;
        self.cps_opt = None;
        self.type_resolution_is_done = false;
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

    pub(super) fn request_symbols(&mut self) -> DocSymbolAnalysisMut {
        if self.syntax_opt.is_some() && self.symbols_opt.is_some() {
            return DocSymbolAnalysisMut {
                syntax: self.syntax_opt.as_mut().unwrap(),
                symbols: self.symbols_opt.as_mut().unwrap(),
            };
        }

        let doc = self.doc;
        let mut listener = ImplNameResolutionListener::default();

        let symbols = {
            let syntax = self.request_syntax();

            let doc_logs = DocLogs::new();
            let (mod_outline, decl_symbols) =
                front::generate_outline(doc, &syntax.tree, &mut listener, &doc_logs.logger());

            let errors = {
                let logs = Logs::new();
                logs.logger().extend_from_doc_logs(doc, doc_logs);
                logs_into_errors(logs, &syntax.tree)
            };

            Symbols {
                mod_outline,
                decl_symbols,
                ty_use_sites: listener.ty_use_sites,
                errors,
            }
        };

        self.symbols_opt = Some(symbols);

        DocSymbolAnalysisMut {
            syntax: self.syntax_opt.as_mut().unwrap(),
            symbols: self.symbols_opt.as_mut().unwrap(),
        }
    }

    pub(super) fn doc_content_analysis_mut(&mut self) -> Option<DocContentAnalysisMut<'_>> {
        Some(DocContentAnalysisMut {
            syntax: self.syntax_opt.as_ref()?,
            symbols: self.symbols_opt.as_mut()?,
            cps: self.cps_opt.as_mut()?,
        })
    }

    pub(super) fn request_cps(
        &mut self,
        mod_outlines: &mut KModOutlines,
    ) -> DocContentAnalysisMut<'_> {
        if self.syntax_opt.is_some() && self.symbols_opt.is_some() && self.cps_opt.is_some() {
            return self.doc_content_analysis_mut().unwrap();
        }

        let doc = self.doc;
        let k_mod = self.mod_opt.unwrap();
        let DocSymbolAnalysisMut { syntax, symbols } = self.request_symbols();

        let mut listener = ImplNameResolutionListener::default();
        let doc_logs = DocLogs::new();
        let logs = Logs::new();

        {
            let mut aliases = take(&mut k_mod.of_mut(mod_outlines).aliases);
            crate::cps::resolve_aliases(&mut aliases, mod_outlines, logs.logger());
            k_mod.of_mut(mod_outlines).aliases = aliases;
        }

        let mod_data = front::convert_to_cps(
            doc,
            k_mod,
            &syntax.tree,
            &symbols.decl_symbols,
            &mod_outlines[k_mod],
            mod_outlines,
            &mut listener,
            &doc_logs.logger(),
        );
        let errors = {
            logs.logger().extend_from_doc_logs(doc, doc_logs);
            logs_into_errors(logs, &syntax.tree)
        };

        self.cps_opt = Some(Cps {
            mod_data,
            ty_use_sites: listener.ty_use_sites,
            errors,
        });

        self.doc_content_analysis_mut().unwrap()
    }

    pub(super) fn resolve_types(&mut self, mod_outlines: &mut KModOutlines) {
        if self.type_resolution_is_done {
            return;
        }

        let k_mod = self.mod_opt.unwrap();
        let DocContentAnalysisMut { syntax, cps, .. } = self.request_cps(mod_outlines);

        let logs = Logs::new();

        resolve_types(
            k_mod,
            k_mod.of(mod_outlines),
            &mut cps.mod_data,
            mod_outlines,
            logs.logger(),
        );
        cps.errors.extend(logs_into_errors(logs, &syntax.tree));
        self.type_resolution_is_done = true;
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
