use super::{lsp_handler::LspHandler, lsp_receiver::LspReceiver, lsp_sender::LspSender};
use jl_compiler::rust_api::LangService;
use std::{
    io::{stdin, stdout},
    path::PathBuf,
};

pub fn start_lsp_server() -> ! {
    let stdin = stdin();
    let stdin = stdin.lock();
    let receiver = LspReceiver::new(stdin);

    let stdout = stdout();
    let stdout = stdout.lock();
    let sender = LspSender::new(stdout);

    let lang_service = LangService::new();
    let handler = LspHandler::new(sender, lang_service);

    handler.main(receiver)
}
