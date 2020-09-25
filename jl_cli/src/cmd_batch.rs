use crate::{
    cmd_build::do_exec_build_cmd,
    cmd_help::string_is_help_flag,
    parse_cmd,
    util::{dyn_error::DynError, package_info::PackageInfo},
    Cmd,
};
use std::{
    collections::HashMap,
    env::Args,
    fmt::Debug,
    io,
    panic::{self, UnwindSafe},
};
use tinyjson::{JsonParser, JsonValue};

fn as_string(value: JsonValue) -> Option<String> {
    match value {
        JsonValue::String(value) => Some(value),
        _ => None,
    }
}

fn as_array(value: JsonValue) -> Option<Vec<JsonValue>> {
    match value {
        JsonValue::Array(vec) => Some(vec),
        _ => None,
    }
}

struct InputRow {
    id: JsonValue,
    args: Vec<String>,
}

impl InputRow {
    fn from_json(value: JsonValue) -> Result<Self, &'static str> {
        let value = match value {
            JsonValue::Object(mut map) => {
                let id = map.remove("id").ok_or("expected id")?;
                let args = map
                    .remove("args")
                    .and_then(|args| as_array(args)?.into_iter().map(as_string).collect())
                    .ok_or("expected args: string[]")?;
                InputRow { id, args }
            }
            _ => return Err("expected {id, args}"),
        };
        Ok(value)
    }
}

struct Action<A> {
    id: JsonValue,
    cmd: Cmd,
    args: A,
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
    let row: InputRow = match JsonParser::new(json.chars()).parse() {
        Ok(row) => InputRow::from_json(row)?,
        Err(err) => return Err(format!("JSON としてパースできません。({})", err).into()),
    };

    let id = row.id;

    let mut args = row.args.into_iter();
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
        let output = {
            let mut o = HashMap::new();
            o.insert("id".to_string(), id);
            o.insert("err".to_string(), JsonValue::String(format!("{:?}", err)));
            JsonValue::Object(o)
        };

        println!("{}", output.stringify().unwrap());
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
