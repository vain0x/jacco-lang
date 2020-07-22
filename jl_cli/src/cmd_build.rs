use crate::util::{dyn_error::DynError, package_info::PackageInfo};
use jl_compiler::rust_api::compile;
use std::{
    env::{self, Args},
    fs,
    io::{self, Read},
    path::{Path, PathBuf},
    process,
};

pub(crate) fn write_build_help() {
    let package_info = PackageInfo::from_env();
    print!(
        include_str!("cmd_build_help.txt"),
        command = package_info.name,
        version = package_info.version
    );
}

fn read_from_stdin(buf: &mut String) -> Result<(), DynError> {
    io::stdin().read_to_string(buf)?;
    Ok(())
}

fn read_from_file(path: &Path, buf: &mut String) -> Result<(), DynError> {
    *buf = fs::read_to_string(path)?;
    Ok(())
}

pub(crate) fn exec_build_cmd(mut args: Args, help: bool) -> Result<(), DynError> {
    if help {
        write_build_help();
        return Ok(());
    }

    let mut src = String::new();
    let output;

    match args.next().as_deref() {
        None => {
            write_build_help();
            return Err("ソースファイルを指定してください。".into());
        }
        Some("-") => {
            let source_path = env::current_dir()?.join("STDIN.jacco");
            read_from_stdin(&mut src)?;
            output = compile(&source_path, &src);
        }
        Some(path) => {
            let source_path = PathBuf::from(path);
            read_from_file(&source_path, &mut src)?;
            output = compile(&source_path, &src);
        }
    }

    print!("{}", output);
    process::exit(0)
}
