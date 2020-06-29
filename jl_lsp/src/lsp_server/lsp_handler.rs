use super::{
    lsp_receiver::LspReceiver, lsp_sender::LspSender, LspMessageOpaque, LspNotification, LspRequest,
};
use crate::sources::{SourceChange, Sources};
use jl_compiler::rust_api::{LangService, Source};
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
    jl_compiler::rust_api::Pos::new(position.line as usize, position.character as usize)
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
    sources: Sources,
    dirty_sources: HashSet<Source>,
    source_changes: Vec<SourceChange>,
}

impl<W: Write> LspHandler<W> {
    pub(crate) fn new(sender: LspSender<W>, service: LangService) -> Self {
        Self {
            sender,
            service,
            sources: Sources::new(),
            dirty_sources: HashSet::new(),
            source_changes: vec![],
        }
    }

    fn notify_source_changes(&mut self) {
        let mut dirty_sources = take(&mut self.dirty_sources);

        self.sources.drain_changes(&mut self.source_changes);
        trace!("source_changes={:?}", self.source_changes);

        for change in self.source_changes.drain(..) {
            match change {
                SourceChange::DidOpen {
                    source,
                    version,
                    text,
                    path,
                } => {
                    self.service.open_doc(source, version, text);
                    dirty_sources.insert(source);
                }
                SourceChange::DidChange {
                    source,
                    version,
                    text,
                } => {
                    self.service.change_doc(source, version, text);
                    dirty_sources.insert(source);
                }
                SourceChange::DidClose { source } => {
                    self.service.close_doc(source);
                    dirty_sources.remove(&source);
                }
            }
        }

        let mut diagnostics = vec![];

        for source in dirty_sources.drain() {
            let url = match self.sources.source_to_url(source) {
                Some(url) => url,
                None => continue,
            };

            let (version_opt, errors) = self.service.validate(source);

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

        self.dirty_sources = dirty_sources;
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
                // definition_provider: Some(true),
                document_highlight_provider: Some(true),
                hover_provider: Some(true),
                // references_provider: Some(true),
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

        self.sources
            .doc_did_open(url, doc.version, Rc::new(doc.text));
        self.notify_source_changes();
    }

    fn text_document_did_change(&mut self, mut params: DidChangeTextDocumentParams) {
        let text = match params.content_changes.as_mut_slice() {
            [content_change] => take(&mut content_change.text),
            _ => return,
        };

        let doc = params.text_document;
        let url = doc.uri;
        let version = doc.version.unwrap_or(0);

        self.sources.doc_did_change(url, version, Rc::new(text));
        self.notify_source_changes();
    }

    fn text_document_did_close(&mut self, params: DidCloseTextDocumentParams) {
        let url = params.text_document.uri;

        self.sources.doc_did_close(url);
        self.notify_source_changes();
    }

    // fn text_document_completion(&mut self, params: CompletionParams) -> CompletionList {
    //     self.service.completion(
    //         params.text_document_position.text_document.uri,
    //         params.text_document_position.position,
    //     )
    // }

    fn completion_item_resolve(&mut self, completion_item: CompletionItem) -> CompletionItem {
        completion_item
    }

    // fn text_document_definition(
    //     &mut self,
    //     params: TextDocumentPositionParams,
    // ) -> lsp_types::GotoDefinitionResponse {
    //     let definitions = self
    //         .service
    //         .definitions(params.text_document.uri, params.position);

    //     if definitions.len() == 1 {
    //         lsp_types::GotoDefinitionResponse::Scalar(definitions.into_iter().next().unwrap())
    //     } else {
    //         lsp_types::GotoDefinitionResponse::Array(definitions)
    //     }
    // }

    fn text_document_highlight(
        &mut self,
        params: TextDocumentPositionParams,
    ) -> Vec<DocumentHighlight> {
        let doc = params.text_document;
        let pos = from_lsp_pos(params.position);

        (|| {
            let uri = doc.uri.clone();
            let source = self.sources.url_to_source(&doc.uri)?;

            let (def_sites, use_sites) = self.service.document_highlight(source, pos)?;

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

        let source = self.sources.url_to_source(&url)?;
        let text = self.service.hover(source, pos)?;
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

    // fn text_document_references(&mut self, params: ReferenceParams) -> Vec<Location> {
    //     self.service.references(
    //         params.text_document_position.text_document.uri,
    //         params.text_document_position.position,
    //         params.context.include_declaration,
    //     )
    // }

    // fn text_document_rename(&mut self, params: RenameParams) -> Option<WorkspaceEdit> {
    //     self.service.rename(
    //         params.text_document_position.text_document.uri,
    //         params.text_document_position.position,
    //         params.new_name,
    //     )
    // }

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
            // "textDocument/definition" => {
            //     let msg =
            //         serde_json::from_str::<LspRequest<TextDocumentPositionParams>>(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.text_document_definition(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
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
            // "textDocument/references" => {
            //     let msg: LspRequest<ReferenceParams> = serde_json::from_str(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.text_document_references(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
            // request::Rename::METHOD => {
            //     let msg: LspRequest<RenameParams> = serde_json::from_str(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.text_document_rename(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
            method => trace!("Unresolved method='{}'", method),
        }
    }

    pub(crate) fn main(mut self, mut receiver: LspReceiver<impl Read>) -> ! {
        loop {
            receiver.read_next(|json| self.did_receive(json));
        }
    }
}
