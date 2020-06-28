use super::{lsp_handler::LspHandler, lsp_receiver::LspReceiver, lsp_sender::LspSender};
use jl_compiler::rust_api::LangService;
use log::LevelFilter;
use simplelog::{Config, WriteLogger};
use std::{
    env::temp_dir,
    fs::OpenOptions,
    io::{stdin, stdout},
    path::PathBuf,
};

pub(crate) fn init_log() {
    let log_filter = if cfg!(debug_assertions) {
        LevelFilter::Trace
    } else {
        LevelFilter::Info
    };

    let file_path = if cfg!(debug_assertions) {
        PathBuf::from("jacco_lsp.log")
    } else {
        temp_dir().join("jacco_lsp.log")
    };

    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(file_path)
        .expect("log file creation");

    WriteLogger::init(log_filter, Config::default(), file).expect("init log");
}

pub fn start_lsp_server() -> ! {
    init_log();

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
