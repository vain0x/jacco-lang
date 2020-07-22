mod cmd_build;
mod cmd_help;
mod cmd_version;

mod util {
    pub(crate) mod dyn_error;
    pub(crate) mod package_info;
}

use cmd_build::exec_build_cmd;
use cmd_help::exec_help_cmd;
use cmd_version::exec_version_cmd;
use env_logger::Env;
use std::{env, process};
use util::dyn_error::DynError;

#[derive(Copy, Clone)]
enum Cmd {
    Build,
    Help,
    Version,
}

const SUBCOMMANDS: &[(Cmd, &str)] = &[
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
    let env = Env::default().default_filter_or("trace");
    env_logger::from_env(env).init();
}

fn dispatch() -> Result<(), DynError> {
    let mut args = env::args();

    // 自分のパスを飛ばす。
    args.next();

    let mut help = false;
    let cmd_result = loop {
        match args.next() {
            None => break Ok(Cmd::Help),
            Some(arg) => match arg.as_str() {
                "-h" | "--help" | "-help" => help = true,
                "-V" | "--version" | "-version" | "version" => break Ok(Cmd::Version),
                _ => break parse_cmd(&arg).ok_or(arg),
            },
        }
    };

    match cmd_result {
        Err(arg) => return Err(format!("サブコマンド '{}' がありません。", arg).into()),
        Ok(cmd) => match cmd {
            Cmd::Build => exec_build_cmd(args, help)?,
            Cmd::Help => exec_help_cmd(),
            Cmd::Version => exec_version_cmd(),
        },
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
