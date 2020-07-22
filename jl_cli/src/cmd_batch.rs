use crate::{
    cmd_build::do_exec_build_cmd,
    parse_cmd,
    util::{dyn_error::DynError, package_info::PackageInfo},
    Cmd,
};
use miniserde::{
    json::{self, Value},
    Deserialize, Serialize,
};
use std::{env::Args, io};

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
            return Err(format!("JSON としてパースできません。({:?})", json).into())
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
        return Err(format!("batch ではサブコマンド '{:?}' を使えません。", cmd_name).into());
    }

    Ok(Action { id, cmd, args })
}

fn exec_action(action: Action<impl Iterator<Item = String>>) {
    let Action { id, cmd, args } = action;

    let result = match cmd {
        Cmd::Build => do_exec_build_cmd(args),
        Cmd::Batch | Cmd::Help | Cmd::Version => unreachable!(),
    };

    if let Err(err) = result {
        let err = Some(format!("{:?}", err.into_inner()));
        println!("{}", json::to_string(&OutputRow { id, err }));
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

pub(crate) fn exec_batch_cmd(_args: Args, help: bool) -> Result<(), DynError> {
    if help {
        write_batch_help();
        return Ok(());
    }

    exec_loop()
}
