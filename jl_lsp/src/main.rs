mod docs;
mod lsp_server;

mod utils {
    mod uri;
    mod watcher;

    pub(crate) use uri::Uri;
}

use std::{
    env::{self, ArgsOs},
    ffi::OsString,
    io::{self, Write},
    path::PathBuf,
    process,
};

enum Arg {
    Help,
    Version,
    Start,
}

fn write_help(w: &mut impl Write) -> io::Result<()> {
    write!(
        w,
        r#"{name} {version}

    USAGE:
        {name} [OPTIONS] [SUBCOMMAND]

    EXAMPLE:
        {name} start

    SUBCOMMANDS:
        start           LSP サーバーとして起動する
        help            ヘルプを表示する (--help と同じ)
        version         バージョン番号を表示する (--version と同じ)

    OPTIONS:
        -h, --help      ヘルプを表示する
        -V, --version   バージョン番号を表示する"#,
        name = env!("CARGO_PKG_NAME"),
        version = get_version()
    )
}

fn get_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn exit_with_help() -> ! {
    let mut stdout = io::stdout();
    write_help(&mut stdout).ok();
    process::exit(1)
}

fn exit_with_version() -> ! {
    eprintln!("{}", get_version());
    process::exit(0)
}

fn parse_args_for_subcommand(args: &mut impl Iterator<Item = OsString>) -> Result<Arg, ()> {
    let subcommand = match args.next() {
        None => return Ok(Arg::Help),
        Some(x) => x,
    };

    match subcommand.to_string_lossy().as_ref() {
        "-h" | "--help" | "help" | "-?" | "/?" => Ok(Arg::Help),
        "-V" | "--version" | "version" => Ok(Arg::Version),
        "start" => Ok(Arg::Start),
        subcommand => {
            eprintln!("Unknown subcommand '{}'.", subcommand);
            Err(())
        }
    }
}

fn init_log() {
    use log::LevelFilter;
    use simplelog::{Config, WriteLogger};
    use std::{env::temp_dir, fs::OpenOptions};

    let log_filter = if cfg!(debug_assertions) {
        LevelFilter::Trace
    } else {
        LevelFilter::Warn
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

fn execute_with_args(mut args: ArgsOs) -> Result<(), ()> {
    args.next();

    match parse_args_for_subcommand(&mut args)? {
        Arg::Help => exit_with_help(),
        Arg::Version => exit_with_version(),
        Arg::Start => lsp_server::start_lsp_server(),
    }
}

fn main() {
    init_log();

    match execute_with_args(env::args_os()) {
        Ok(()) => {}
        Err(()) => exit_with_help(),
    }
}
