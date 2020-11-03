use crate::util::{dyn_error::DynError, package_info::PackageInfo};
use jl_compiler::rust_api::dump_syntax;
use std::{
    env::{self, Args},
    fs,
    io::{self, Read},
    path::PathBuf,
    process,
};

pub(crate) fn write_dump_syntax_help() {
    let package_info = PackageInfo::from_env();
    print!(
        include_str!("cmd_dump_syntax_help.txt"),
        command = package_info.name,
        version = package_info.version
    );
}

pub(crate) fn do_exec_dump_syntax_cmd(
    mut args: impl Iterator<Item = String>,
) -> Result<(), DynError> {
    let mut use_stdin = false;
    let mut source_path_opt = None;

    // or stdout
    let mut output_path_opt = None;

    loop {
        let arg = match args.next() {
            Some(arg) => arg,
            None => break,
        };

        match arg.as_str() {
            "-o" | "--out" => {
                let output_path = args
                    .next()
                    .ok_or_else(|| DynError::from("-o/--out の後にファイル名がありません。"))?;
                output_path_opt = Some(output_path);
            }
            "-" => use_stdin = true,
            _ => {
                let old = source_path_opt.replace(arg);
                if old.is_some() {
                    return Err("ファイル名を2つ以上、指定することはできません。".into());
                }
            }
        }
    }

    let syntax_dump = if use_stdin {
        let source_path = env::current_dir()?.join("STDIN.jacco");
        let mut src = String::new();
        io::stdin().read_to_string(&mut src)?;
        dump_syntax(&source_path, &src)
    } else if let Some(source_path) = &source_path_opt {
        let source_path = PathBuf::from(source_path);
        let src = fs::read_to_string(&source_path)?;
        dump_syntax(&source_path, &src)
    } else {
        return Err("ソースファイルが指定されていません。".into());
    };

    if let Some(output_file) = output_path_opt {
        fs::write(output_file, syntax_dump.tree)?;
    } else {
        print!("{}", syntax_dump.tree);
    }

    for (range, msg) in syntax_dump.errors {
        let file_path = match &source_path_opt {
            Some(it) => format!("{}:", it.as_str()),
            None => String::new(),
        };

        eprintln!("ERROR: {}{:?} {}", file_path, range, msg);
    }

    Ok(())
}

pub(crate) fn exec_dump_syntax_cmd(args: Args, help: bool) -> Result<(), DynError> {
    if help {
        write_dump_syntax_help();
        return Ok(());
    }

    do_exec_dump_syntax_cmd(args)?;
    process::exit(0)
}
