mod lsp_handler;
mod lsp_main;
mod lsp_receiver;
mod lsp_sender;

use serde::{Deserialize, Serialize};

pub(crate) use lsp_main::start_lsp_server;

#[derive(Serialize, Deserialize)]
pub(super) struct LspRequest<Params> {
    pub(crate) jsonrpc: String,
    pub(crate) id: i64,
    pub(crate) method: String,
    pub(crate) params: Params,
}

#[derive(Serialize, Deserialize)]
pub(super) struct LspResponse<Result> {
    pub(crate) jsonrpc: String,
    pub(crate) id: i64,
    pub(crate) result: Result,
}

#[derive(Serialize, Deserialize)]
pub(super) struct LspNotification<Params> {
    pub(crate) jsonrpc: String,
    pub(crate) method: String,
    pub(crate) params: Params,
}

/// LSP message (request or notification) without results/params
/// just for deserialization.
#[derive(Deserialize)]
pub(super) struct LspMessageOpaque {
    pub(crate) method: String,
}
