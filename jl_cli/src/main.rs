mod cmd_batch;
mod cmd_build;
mod cmd_help;
mod cmd_version;

mod util {
    pub(crate) mod dyn_error;
    pub(crate) mod package_info;
}

use cmd_batch::exec_batch_cmd;
use cmd_build::exec_build_cmd;
use cmd_help::exec_help_cmd;
use cmd_version::exec_version_cmd;
use std::{env, process};
use util::dyn_error::DynError;

#[derive(Copy, Clone)]
enum Cmd {
    Batch,
    Build,
    Help,
    Version,
}

const SUBCOMMANDS: &[(Cmd, &str)] = &[
    (Cmd::Batch, "batch"),
    (Cmd::Build, "build"),
    (Cmd::Help, "help"),
    (Cmd::Version, "version"),
];

pub(crate) fn parse_cmd(s: &str) -> Option<Cmd> {
    SUBCOMMANDS
        .iter()
        .find_map(|&(cmd, name)| if s == name { Some(cmd) } else { None })
}

fn init_log() {
    let env = env_logger::Env::default();
    let mut builder = env_logger::from_env(env);

    if let Some("off") = std::env::var("RUST_LOG_TIMESTAMP").ok().as_deref() {
        builder.format_timestamp(None);
    }

    builder.init();
}

fn dispatch() -> Result<(), DynError> {
    let mut args = env::args();
    args.next();

    let mut help = false;
    let cmd_result = loop {
        let arg = match args.next() {
            Some(arg) => arg,
            None => break Ok(Cmd::Help),
        };

        match arg.as_str() {
            "-h" | "--help" | "-help" => help = true,
            "-V" | "--version" | "-version" | "version" => break Ok(Cmd::Version),
            _ => break parse_cmd(&arg).ok_or(arg),
        }
    };

    match cmd_result {
        Ok(cmd) => match cmd {
            Cmd::Batch => exec_batch_cmd(args, help)?,
            Cmd::Build => exec_build_cmd(args, help)?,
            Cmd::Help => exec_help_cmd(),
            Cmd::Version => exec_version_cmd(),
        },
        Err(arg) => return Err(format!("サブコマンド '{}' がありません。", arg).into()),
    }
    Ok(())
}

fn main() {
    init_log();

    match dispatch() {
        Ok(()) => {}
        Err(err) => {
            log::error!("{:?}", err.into_inner());
            process::exit(1)
        }
    }
}
