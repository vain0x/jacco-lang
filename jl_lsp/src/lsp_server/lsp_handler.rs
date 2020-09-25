use super::{
    lsp_receiver::LspReceiver, lsp_sender::LspSender, LspMessageOpaque, LspNotification, LspRequest,
};
use crate::{
    docs::{DocChange, Docs},
    utils::json::*,
};
use bumpalo::Bump;
use jl_compiler::rust_api::{Content, Doc, LangService};
use log::trace;
use lsp_types::request::*;
use lsp_types::*;
use std::{
    collections::HashSet,
    io::{Read, Write},
    mem::{replace, take},
    process,
    rc::Rc,
};

fn pos_to_json(pos: Position) -> JsonValue<'static> {
    let mut o = JsonObject::new();
    o.insert("line").number(pos.line as f64);
    o.insert("character").number(pos.character as f64);
    o.into_value()
}

fn range_to_json(range: Range) -> JsonValue<'static> {
    let mut o = JsonObject::new();
    o.insert("start").value(pos_to_json(range.start));
    o.insert("end").value(pos_to_json(range.end));
    o.into_value()
}

struct Diagnostic<'a> {
    severity: DiagnosticSeverity,
    range: Range,
    message: &'a str,
}

#[derive(Default)]
struct Diagnostics<'a> {
    array: JsonArray<'a>,
}

impl<'a> Diagnostics<'a> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn push(&mut self, diagnostic: Diagnostic<'a>) {
        let Diagnostic {
            severity,
            range,
            message,
        } = diagnostic;

        let item = {
            let mut o = JsonObject::new();
            o.insert("severity").number(severity as i32 as f64);
            o.insert("range").value(range_to_json(range));
            o.insert("message").string(message);
            o.into_value()
        };

        self.array.push().value(item);
    }

    pub(crate) fn split_off(&mut self, at: usize) -> JsonValue<'a> {
        let array = self.array.0.split_off(at);
        JsonValue::Array(JsonArray(array))
    }
}

// 参考: https://doc.rust-lang.org/cargo/reference/environment-variables.html#environment-variables-cargo-sets-for-crates
fn get_package_name() -> &'static str {
    env!("CARGO_PKG_NAME")
}

fn get_package_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn lsp_display_name() -> String {
    format!("{} v{}", get_package_name(), get_package_version())
}

fn from_lsp_pos(position: Position) -> jl_compiler::rust_api::TPos16 {
    jl_compiler::rust_api::TPos16::new(position.line as u32, position.character as u32)
}

fn to_lsp_pos(pos: jl_compiler::rust_api::TPos) -> Position {
    Position {
        line: pos.row as u64,
        character: pos.column16 as u64,
    }
}

fn to_lsp_range(range: jl_compiler::rust_api::TRange) -> Range {
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

    fn notify_doc_changes<'a>(&mut self, bump: &'a Bump) {
        let mut dirty_docs = take(&mut self.dirty_docs);

        self.docs.drain_changes(&mut self.doc_changes);

        for change in self.doc_changes.drain(..) {
            match change {
                DocChange::DidOpen {
                    doc,
                    version,
                    text,
                    path,
                } => {
                    self.service.open_doc(doc, path, version, text);
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

        let mut diagnostics = Diagnostics::new();

        for doc in dirty_docs.drain() {
            let url = match self.docs.doc_to_url(doc) {
                Some(url) => url,
                None => continue,
            };

            let (version_opt, errors) = self.service.validate(doc);

            for (range, message) in errors {
                diagnostics.push(Diagnostic {
                    severity: DiagnosticSeverity::Error,
                    range: to_lsp_range(range),
                    message: bump.alloc_str(&message),
                });
            }

            self.send_publish_diagnostics(url, version_opt, &mut diagnostics, bump);
        }

        self.dirty_docs = dirty_docs;
    }

    /// <https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#initialize>
    fn initialize(&mut self) -> JsonValue<'static> {
        // ServerCapabilities
        let capabilities = {
            // TextDocumentSync
            let text_document_sync = {
                let mut o = JsonObject::new();
                o.insert("openClose").boolean(true);
                o.insert("change").number(1.0); // TextDocumentSyncKind::Full
                o.into_value()
            };

            // TODO: completion
            // completion_provider: Some(CompletionOptions {
            //     resolve_provider: Some(true),
            //     trigger_characters: None,
            //     work_done_progress_options: WorkDoneProgressOptions::default(),
            // }),

            let mut o = JsonObject::new();
            o.insert("textDocumentSync").value(text_document_sync);
            o.insert("definitionProvider").boolean(true);
            // o.insert("documentHighlightProvider").boolean(true);
            // o.insert("hoverProvider").boolean(true);
            // o.insert("referencesProvider").boolean(true);
            // o.insert("renameProvider").boolean(true);
            o.into_value()
        };

        let server_info = {
            let mut o = JsonObject::new();
            o.insert("name").string(get_package_name());
            o.insert("version").string(get_package_version());
            o.into_value()
        };

        let result = {
            let mut o = JsonObject::new();
            o.insert("capabilities").value(capabilities);
            o.insert("server_info").value(server_info);
            o.into_value()
        };
        result
    }

    fn did_initialize(&mut self) {
        self.service.did_initialize();
    }

    fn shutdown(&mut self) {
        self.service.shutdown();
    }

    fn did_exit(&mut self) {
        process::exit(0)
    }

    fn send_publish_diagnostics(
        &mut self,
        url: Url,
        version_opt: Option<i64>,
        diagnostics: &mut Diagnostics<'_>,
        bump: &Bump,
    ) {
        // PublishDiagnosticsParams
        let params = {
            let mut o = JsonObject::new();
            o.insert("uri").string(bump.alloc_str(url.as_str()));
            if let Some(version) = version_opt {
                o.insert("version").number(version as f64);
            }
            o.insert("diagnostics").value(diagnostics.split_off(0));
            o.into_value()
        };

        self.sender
            .send_notification("textDocument/publishDiagnostics", params);
    }

    // params: DidOpenTextDocumentParams
    fn text_document_did_open<'a>(&mut self, params: &JsonValue, bump: &'a Bump) {
        let (url, text, version) = {
            let doc = params.of_object().get("textDocument").of_object();

            let url = doc.get("uri").of_string().parse::<Url>().unwrap();
            let text = doc.get("text").of_string();
            let version = doc.get("version").of_number() as i64;
            (url, text, version)
        };

        self.docs
            .doc_did_open(url, version, Rc::new(text.to_string()));
        self.notify_doc_changes(bump);
    }

    // params: DidChangeTextDocumentParams
    fn text_document_did_change(&mut self, params: &JsonValue, bump: &Bump) {
        let text = match params
            .of_object()
            .get("contentChanges")
            .of_array()
            .as_slice()
        {
            [change] => change.of_object().get("text").of_string(),
            _ => return,
        };

        let (url, version) = {
            let doc = params.of_object().get("textDocument").of_object();

            let url = doc.get("uri").of_string().parse::<Url>().unwrap();
            let version = doc.get("version").of_number() as i64;
            (url, version)
        };

        self.docs
            .doc_did_change(url, version, Rc::new(text.to_string()));
        self.notify_doc_changes(bump);
    }

    // params: DidCloseTextDocumentParams
    fn text_document_did_close(&mut self, params: &JsonValue, bump: &Bump) {
        let url = params
            .of_object()
            .get("textDocument")
            .of_object()
            .get("uri")
            .of_string()
            .parse::<Url>()
            .unwrap();

        self.docs.doc_did_close(url);
        self.notify_doc_changes(bump);
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

    // params: TextDocumentPositionParams
    // result: Location | Location[] | LocationLink[] | null
    fn text_document_definition<'a>(
        &mut self,
        params: &JsonValue,
        bump: &'a Bump,
    ) -> JsonValue<'a> {
        fn pos_from_json(value: &JsonValue) -> jl_compiler::rust_api::TPos16 {
            jl_compiler::rust_api::TPos16::new(
                value.of_object().get("line").of_number() as u32,
                value.of_object().get("character").of_number() as u32,
            )
        }

        fn location_to_json(location: Location, bump: &Bump) -> JsonValue {
            let mut o = JsonObject::new();
            o.insert("uri")
                .string(bump.alloc_str(location.uri.as_str()));
            o.insert("range").value(range_to_json(location.range));
            o.into_value()
        }

        let (uri, pos) = {
            let uri = params
                .of_object()
                .get("textDocument")
                .of_object()
                .get("uri")
                .of_string()
                .parse::<Url>()
                .unwrap();
            let pos = pos_from_json(params.of_object().get("position"));
            (uri, pos)
        };

        (|| {
            let doc = self.docs.url_to_doc(&uri)?;
            let definitions = self
                .service
                .definitions(doc, pos)?
                .into_iter()
                .map(|location| {
                    let range = to_lsp_range(location.range());
                    Location::new(uri.clone(), range)
                })
                .collect::<Vec<_>>();
            let result = match definitions.as_slice() {
                [location] => location_to_json(location.clone(), bump),
                _ => JsonArray::from_iter(
                    definitions
                        .into_iter()
                        .map(|location| location_to_json(location, bump)),
                )
                .into_value(),
            };
            Some(result)
        })()
        .unwrap_or(JsonValue::Null)
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
        let content = self.service.hover(doc, pos)?;
        let hover = Hover {
            contents: to_hover_content(content)?,
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
        let bump = bumpalo::Bump::new();

        let mut slot = tinyjson::JsonValue::Null;
        let msg = JsonValue::parse(json, &mut slot);
        let params = || msg.of_object().get("params");
        let msg_id = || msg.of_object().get("id").of_number() as i64;

        match msg.of_object().get("method").of_string() {
            "initialize" => {
                let result = self.initialize();
                self.sender.send_response(msg_id(), result);
            }
            "initialized" => {
                self.did_initialize();
            }
            "shutdown" => {
                self.shutdown();
                self.sender.send_response(msg_id(), JsonValue::Null);
            }
            "exit" => {
                self.did_exit();
            }
            "textDocument/didOpen" => {
                self.text_document_did_open(params(), &bump);
            }
            "textDocument/didChange" => {
                self.text_document_did_change(params(), &bump);
            }
            "textDocument/didClose" => {
                self.text_document_did_close(params(), &bump);
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
            "textDocument/definition" => {
                let result = self.text_document_definition(params(), &bump);
                self.sender.send_response(msg_id(), result);
            }
            "textDocument/documentHighlight" => {
                // let msg =
                //     serde_json::from_str::<LspRequest<TextDocumentPositionParams>>(json).unwrap();
                // let msg_id = msg.id;
                // let response = self.text_document_highlight(msg.params);
                // self.sender.send_response(msg_id, response);
            }
            "textDocument/hover" => {
                // let msg: LspRequest<TextDocumentPositionParams> =
                //     serde_json::from_str(json).unwrap();
                // let msg_id = msg.id;
                // let response = self.text_document_hover(msg.params);
                // self.sender.send_response(msg_id, response);
            }
            // request::PrepareRenameRequest::METHOD => {
            //     let msg: LspRequest<TextDocumentPositionParams> =
            //         serde_json::from_str(json).unwrap();
            //     let msg_id = msg.id;
            //     let response = self.text_document_prepare_rename(msg.params);
            //     self.sender.send_response(msg_id, response);
            // }
            "textDocument/references" => {
                // let msg: LspRequest<ReferenceParams> = serde_json::from_str(json).unwrap();
                // let msg_id = msg.id;
                // let response = self.text_document_references(msg.params);
                // self.sender.send_response(msg_id, response);
            }
            "textDocument/rename" => {
                // let msg: LspRequest<RenameParams> = serde_json::from_str(json).unwrap();
                // let msg_id = msg.id;
                // let response = self.text_document_rename(msg.params);
                // self.sender.send_response(msg_id, response);
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

fn do_convert_to_hover_content(content: Content, out: &mut Vec<MarkedString>) {
    match content {
        Content::String(text) => out.push(MarkedString::String(text)),
        Content::JaccoCode(code) => out.push(MarkedString::LanguageString(LanguageString {
            language: "jacco".to_string(),
            value: code,
        })),
        Content::Concat(contents) => {
            for content in contents {
                do_convert_to_hover_content(content, out);
            }
        }
    }
}

fn to_hover_content(content: Content) -> Option<HoverContents> {
    let mut items = vec![];
    do_convert_to_hover_content(content, &mut items);

    match items.as_mut_slice() {
        [] => None,
        [item] => Some(HoverContents::Scalar(replace(
            item,
            MarkedString::String(String::new()),
        ))),
        _ => Some(HoverContents::Array(items)),
    }
}
