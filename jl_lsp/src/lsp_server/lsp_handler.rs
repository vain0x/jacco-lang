use super::{
    lsp_receiver::LspReceiver, lsp_sender::LspSender, LspMessageOpaque, LspNotification, LspRequest,
};
use crate::docs::{DocChange, Docs};
use jl_compiler::rust_api::{Doc, LangService};
use log::trace;
use lsp_types::request::*;
use lsp_types::*;
use std::{
    collections::HashSet,
    io::{Read, Write},
    mem::take,
    process,
    rc::Rc,
};

fn get_package_name() -> &'static str {
    env!("CARGO_PKG_NAME")
}

fn get_package_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn lsp_display_name() -> String {
    format!("{} v{}", get_package_name(), get_package_version())
}

fn from_lsp_pos(position: Position) -> jl_compiler::rust_api::Pos {
    jl_compiler::rust_api::Pos::new(position.line as usize, position.character as usize, 0)
}

fn to_lsp_pos(pos: jl_compiler::rust_api::Pos) -> Position {
    Position {
        line: pos.line() as u64,
        character: pos.character() as u64,
    }
}

fn to_lsp_range(range: jl_compiler::rust_api::Range) -> Range {
    Range {
        start: to_lsp_pos(range.start()),
        end: to_lsp_pos(range.end()),
    }
}

pub(crate) struct LspHandler<W: Write> {
    sender: LspSender<W>,
    service: LangService,
    docs: Docs,
    dirty_docs: HashSet<Doc>,
    doc_changes: Vec<DocChange>,
}

impl<W: Write> LspHandler<W> {
    pub(crate) fn new(sender: LspSender<W>, service: LangService) -> Self {
        Self {
            sender,
            service,
            docs: Docs::new(),
            dirty_docs: HashSet::new(),
            doc_changes: vec![],
        }
    }

    fn notify_doc_changes(&mut self) {
        let mut dirty_docs = take(&mut self.dirty_docs);

        self.docs.drain_changes(&mut self.doc_changes);
        trace!("doc_changes={:?}", self.doc_changes);

        for change in self.doc_changes.drain(..) {
            match change {
                DocChange::DidOpen {
                    doc,
                    version,
                    text,
                    path: _,
                } => {
                    self.service.open_doc(doc, version, text);
                    dirty_docs.insert(doc);
                }
                DocChange::DidChange { doc, version, text } => {
                    self.service.change_doc(doc, version, text);
                    dirty_docs.insert(doc);
                }
                DocChange::DidClose { doc } => {
                    self.service.close_doc(doc);
                    dirty_docs.remove(&doc);
                }
            }
        }

        let mut diagnostics = vec![];

        for doc in dirty_docs.drain() {
            let url = match self.docs.doc_to_url(doc) {
                Some(url) => url,
                None => continue,
            };

            let (version_opt, errors) = self.service.validate(doc);

            for (range, message) in errors {
                diagnostics.push(Diagnostic {
                    severity: Some(DiagnosticSeverity::Error),
                    range: to_lsp_range(range),
                    message,
                    source: Some(lsp_display_name()),
                    ..Diagnostic::default()
                });
            }

            self.send_publish_diagnostics(url, version_opt, diagnostics.split_off(0));
        }

        self.dirty_docs = dirty_docs;
    }

    fn initialize<'a>(&'a mut self, _params: InitializeParams) -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::Full),
                        ..TextDocumentSyncOptions::default()
                    },
                )),
                // completion_provider: Some(CompletionOptions {
                //     resolve_provider: Some(true),
                //     trigger_characters: None,
                //     work_done_progress_options: WorkDoneProgressOptions::default(),
                // }),
                definition_provider: Some(true),
                document_highlight_provider: Some(true),
                hover_provider: Some(true),
                references_provider: Some(true),
                rename_provider: Some(RenameProviderCapability::Simple(true)),
                // rename_provider: Some(RenameProviderCapability::Options(RenameOptions {
                //     prepare_provider: Some(true),
                //     work_done_progress_options: WorkDoneProgressOptions::default(),
                // })),
                ..ServerCapabilities::default()
            },
            // 参考: https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates
            server_info: Some(ServerInfo {
                name: get_package_name().to_string(),
                version: Some(get_package_version().to_string()),
            }),
        }
    }

    fn did_initialize(&mut self) {
        self.service.did_initialize();
    }

    fn shutdown(&mut self) {
        self.service.shutdown();
    }

    fn did_exit(&mut self, _json: &str) {
        process::exit(0)
    }

    fn send_publish_diagnostics(
        &mut self,
        url: Url,
        version_opt: Option<i64>,
        diagnostics: Vec<Diagnostic>,
    ) {
        self.sender.send_notification(
            "textDocument/publishDiagnostics",
            PublishDiagnosticsParams {
                uri: url,
                version: version_opt,
                diagnostics,
            },
        );
    }

    fn text_document_did_open(&mut self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        let url = doc.uri;

        self.docs.doc_did_open(url, doc.version, Rc::new(doc.text));
        self.notify_doc_changes();
    }

    fn text_document_did_change(&mut self, mut params: DidChangeTextDocumentParams) {
        let text = match params.content_changes.as_mut_slice() {
            [content_change] => take(&mut content_change.text),
            _ => return,
        };

        let doc = params.text_document;
        let url = doc.uri;
        let version = doc.version.unwrap_or(0);

        self.docs.doc_did_change(url, version, Rc::new(text));
        self.notify_doc_changes();
    }

    fn text_document_did_close(&mut self, params: DidCloseTextDocumentParams) {
        let url = params.text_document.uri;

        self.docs.doc_did_close(url);
        self.notify_doc_changes();
    }

    // fn text_document_completion(&mut self, params: CompletionParams) -> CompletionList {
    //     self.service.completion(
    //         params.text_document_position.text_document.uri,
    //         params.text_document_position.position,
    //     )
    // }

    #[allow(unused)]
    fn completion_item_resolve(&mut self, completion_item: CompletionItem) -> CompletionItem {
        completion_item
    }

    fn text_document_definition(
        &mut self,
        params: TextDocumentPositionParams,
    ) -> lsp_types::GotoDefinitionResponse {
        let doc = params.text_document;
        let pos = from_lsp_pos(params.position);

        (|| {
            let uri = doc.uri.clone();
            let doc = self.docs.url_to_doc(&doc.uri)?;
            let definitions = self
                .service
                .definitions(doc, pos)?
                .into_iter()
                .map(|location| {
                    let range = to_lsp_range(location.range());
                    Location::new(uri.clone(), range)
                })
                .collect::<Vec<_>>();
            let response = if definitions.len() == 1 {
                lsp_types::GotoDefinitionResponse::Scalar(definitions.into_iter().next().unwrap())
            } else {
                lsp_types::GotoDefinitionResponse::Array(definitions)
            };
            Some(response)
        })()
        .unwrap_or_else(|| lsp_types::GotoDefinitionResponse::Array(vec![]))
    }

    fn text_document_highlight(
        &mut self,
        params: TextDocumentPositionParams,
    ) -> Vec<DocumentHighlight> {
        let doc = params.text_document;
        let pos = from_lsp_pos(params.position);

        (|| {
            let doc = self.docs.url_to_doc(&doc.uri)?;

            let (def_sites, use_sites) = self.service.document_highlight(doc, pos)?;

            let def_highlights = def_sites.into_iter().map(|range| {
                let range = to_lsp_range(range);
                let kind = Some(DocumentHighlightKind::Write);
                DocumentHighlight { kind, range }
            });
            let use_highlights = use_sites.into_iter().map(|range| {
                let range = to_lsp_range(range);
                let kind = Some(DocumentHighlightKind::Read);
                DocumentHighlight { kind, range }
            });
            let highlights = def_highlights.chain(use_highlights).collect();
            Some(highlights)
        })()
        .unwrap_or(vec![])
    }

    fn text_document_hover(&mut self, params: TextDocumentPositionParams) -> Option<Hover> {
        let doc = params.text_document;
        let url = doc.uri;
        let pos = from_lsp_pos(params.position);

        let doc = self.docs.url_to_doc(&url)?;
        let text = self.service.hover(doc, pos)?;
        let hover = Hover {
            contents: HoverContents::Scalar(MarkedString::String(text)),
            range: None,
        };
        Some(hover)
    }

    // fn text_document_prepare_rename(
    //     &mut self,
    //     params: TextDocumentPositionParams,
    // ) -> Option<PrepareRenameResponse> {
    //     self.service
    //         .prepare_rename(params.text_document.uri, params.position)
    // }

    fn text_document_references(&mut self, params: ReferenceParams) -> Vec<Location> {
        let uri = params.text_document_position.text_document.uri;
        let pos = from_lsp_pos(params.text_document_position.position);
        let include_declaration = params.context.include_declaration;

        (|| {
            let doc = self.docs.url_to_doc(&uri)?;
            let refs = self.service.references(doc, pos, include_declaration)?;
            Some(
                refs.into_iter()
                    .map(|location| {
                        let range = to_lsp_range(location.range());
                        Location::new(uri.clone(), range)
                    })
                    .collect(),
            )
        })()
        .unwrap_or(vec![])
    }

    fn text_document_rename(&mut self, params: RenameParams) -> Option<WorkspaceEdit> {
        let uri = params.text_document_position.text_document.uri;
        let pos = from_lsp_pos(params.text_document_position.position);
        let new_name = params.new_name;

        let doc = self.docs.url_to_doc(&uri)?;
        let edits = self.service.rename(doc, pos, new_name)?;
        let document_edits = edits
            .into_iter()
            .map(|(location, version, new_text)| TextDocumentEdit {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: Some(version),
                },
                edits: vec![TextEdit {
                    range: to_lsp_range(location.range()),
                    new_text,
                }],
            })
            .collect();
        let workspace_edit = WorkspaceEdit {
            document_changes: Some(DocumentChanges::Edits(document_edits)),
            ..WorkspaceEdit::default()
        };
        Some(workspace_edit)
    }

    fn did_receive(&mut self, json: &str) {
        let msg = serde_json::from_str::<LspMessageOpaque>(json).unwrap();

        match msg.method.as_str() {
            "initialize" => {
                let msg = serde_json::from_str::<LspRequest<InitializeParams>>(json).unwrap();
                let (params, msg_id) = (msg.params, msg.id);
                let response = self.initialize(params);
                self.sender.send_response(msg_id, response);
            }
            "initialized" => {
                self.did_initialize();
            }
            "shutdown" => {
                let msg = serde_json::from_str::<LspRequest<()>>(json).unwrap();
                self.shutdown();
                self.sender.send_response(msg.id, ());
            }
            "exit" => {
                self.did_exit(json);
            }
            "textDocument/didOpen" => {
                let msg: LspNotification<DidOpenTextDocumentParams> =
                    serde_json::from_str(&json).expect("didOpen msg");
                self.text_document_did_open(msg.params);
            }
            "textDocument/didChange" => {
                let msg: LspNotification<DidChangeTextDocumentParams> =
                    serde_json::from_str(&json).expect("didChange msg");
                self.text_document_did_change(msg.params);
            }
            "textDocument/didClose" => {
                let msg = serde_json::from_str::<LspNotification<DidCloseTextDocumentParams>>(json)
                    .unwrap();
                self.text_document_did_close(msg.params);
            }
            // "textDocument/completion" => {
            //     let msg = serde_json::from_str::<LspRequest<CompletionParams>>(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.text_document_completion(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
            // "completionItem/resolve" => {
            //     let msg = serde_json::from_str::<LspRequest<CompletionItem>>(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.completion_item_resolve(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
            GotoDefinition::METHOD => {
                let msg =
                    serde_json::from_str::<LspRequest<TextDocumentPositionParams>>(json).unwrap();
                let msg_id = msg.id;
                let response = self.text_document_definition(msg.params);
                self.sender.send_response(msg_id, response);
            }
            DocumentHighlightRequest::METHOD => {
                let msg =
                    serde_json::from_str::<LspRequest<TextDocumentPositionParams>>(json).unwrap();
                let msg_id = msg.id;
                let response = self.text_document_highlight(msg.params);
                self.sender.send_response(msg_id, response);
            }
            HoverRequest::METHOD => {
                let msg: LspRequest<TextDocumentPositionParams> =
                    serde_json::from_str(json).unwrap();
                let msg_id = msg.id;
                let response = self.text_document_hover(msg.params);
                self.sender.send_response(msg_id, response);
            }
            // request::PrepareRenameRequest::METHOD => {
            //     let msg: LspRequest<TextDocumentPositionParams> =
            //         serde_json::from_str(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.text_document_prepare_rename(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
            References::METHOD => {
                let msg: LspRequest<ReferenceParams> = serde_json::from_str(json).unwrap();
                let msg_id = msg.id;
                let response = self.text_document_references(msg.params);
                self.sender.send_response(msg_id, response);
            }
            Rename::METHOD => {
                let msg: LspRequest<RenameParams> = serde_json::from_str(json).unwrap();
                let msg_id = msg.id;
                let response = self.text_document_rename(msg.params);
                self.sender.send_response(msg_id, response);
            }
            method => trace!("Unresolved method='{}'", method),
        }
    }

    pub(crate) fn main(mut self, mut receiver: LspReceiver<impl Read>) -> ! {
        loop {
            receiver.read_next(|json| self.did_receive(json));
        }
    }
}
