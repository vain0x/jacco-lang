use crate::{
    cmd_build::do_exec_build_cmd,
    cmd_help::string_is_help_flag,
    parse_cmd,
    util::{dyn_error::DynError, package_info::PackageInfo},
    Cmd,
};
use miniserde::{
    json::{self, Value},
    Deserialize, Serialize,
};
use panic::UnwindSafe;
use std::{env::Args, fmt::Debug, io, panic};

#[derive(Deserialize, Debug)]
struct InputRow {
    id: Option<Value>,
    args: Option<Vec<String>>,
}

struct Action<A> {
    id: Option<Value>,
    cmd: Cmd,
    args: A,
}

#[derive(Serialize, Debug)]
struct OutputRow {
    id: Option<Value>,
    err: Option<String>,
}

fn write_batch_help() {
    let package_info = PackageInfo::from_env();
    print!(
        include_str!("cmd_batch_help.txt"),
        command = package_info.name,
        version = package_info.version,
    );
}

fn parse_action(json: &str) -> Result<Action<impl Iterator<Item = String>>, DynError> {
    let row: InputRow = match json::from_str(json) {
        Ok(row) => row,
        Err(miniserde::Error) => {
            return Err(format!("JSON としてパースできません。({})", json).into())
        }
    };

    let id = row.id;

    let mut args = row.args.into_iter().flatten();
    let cmd_name = match args.next() {
        Some(cmd) => cmd,
        None => return Err("args は空にできません。".into()),
    };

    let cmd = match parse_cmd(&cmd_name) {
        Some(cmd) => cmd,
        None => return Err(format!("サブコマンド '{}' はありません。", cmd_name).into()),
    };

    if let Cmd::Batch | Cmd::Help | Cmd::Version = cmd {
        return Err(format!("batch ではサブコマンド '{}' を使えません。", cmd_name).into());
    }

    Ok(Action { id, cmd, args })
}

fn exec_action(action: Action<impl Iterator<Item = String> + UnwindSafe>) {
    let Action { id, cmd, args } = action;

    let caught = panic::catch_unwind(|| match cmd {
        Cmd::Build => do_exec_build_cmd(args),
        Cmd::Batch | Cmd::Help | Cmd::Version => unreachable!(),
    });

    let result: Result<(), Box<dyn Debug + Send + 'static>> = match caught {
        Ok(Ok(())) => Ok(()),
        Ok(Err(err)) => Err(err.into_inner()),
        Err(err) => Err(Box::new(err)),
    };

    if let Err(err) = result {
        println!("{{\"id\":{},\"err\":{:?}}}", json::to_string(&id), err);
    }
}

fn exec_loop() -> Result<(), DynError> {
    let mut line = String::new();
    io::stdin().read_line(&mut line)?;

    if line.trim().is_empty() {
        return Ok(());
    }

    let action = parse_action(&line)?;

    let ((), result) = rayon::join(move || exec_action(action), exec_loop);
    result
}

pub(crate) fn exec_batch_cmd(mut args: Args, help: bool) -> Result<(), DynError> {
    if help || args.any(|arg| string_is_help_flag(&arg)) {
        write_batch_help();
        return Ok(());
    }

    exec_loop()
}
