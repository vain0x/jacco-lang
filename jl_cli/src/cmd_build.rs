use crate::util::{dyn_error::DynError, package_info::PackageInfo};
use jl_compiler::rust_api::{compile, compile_v2};
use std::{
    env::{self, Args},
    fs,
    io::{self, Read},
    path::PathBuf,
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

pub(crate) fn do_exec_build_cmd(mut args: impl Iterator<Item = String>) -> Result<(), DynError> {
    let mut use_stdin = false;
    let mut source_path_opt = None;
    // or stdout
    let mut output_path_opt = None;
    let mut v2 = false;

    loop {
        let arg = match args.next() {
            Some(arg) => arg,
            None => break,
        };

        match arg.as_str() {
            "--v2" => v2 = true,
            "-o" | "--out" => {
                let output_path = args
                    .next()
                    .ok_or_else(|| DynError::from("-o/--out の後にファイル名がありません。"))?;
                output_path_opt = Some(output_path);
            }
            "-" => use_stdin = true,
            _ => source_path_opt = Some(arg),
        }
    }

    let output_opt = if use_stdin {
        let source_path = env::current_dir()?.join("STDIN.jacco");
        let mut src = String::new();
        io::stdin().read_to_string(&mut src)?;
        compile_with_version(&source_path, &src, v2)
    } else if let Some(source_path) = source_path_opt {
        let source_path = PathBuf::from(source_path);
        let src = fs::read_to_string(&source_path)?;
        compile_with_version(&source_path, &src, v2)
    } else {
        return Err("ソースファイルが指定されていません。".into());
    };

    let output = match output_opt {
        Some(it) => it,
        None => {
            if let Some(output_file) = output_path_opt {
                fs::write(output_file, "").ok();
            }
            return Err("コンパイルエラーが発生しました".into());
        }
    };

    if let Some(output_file) = output_path_opt {
        fs::write(output_file, output)?;
    } else {
        print!("{}", output);
    }
    Ok(())
}

fn compile_with_version(source_path: &PathBuf, src: &String, v2: bool) -> Option<String> {
    if v2 {
        compile_v2(source_path, src)
    } else {
        Some(compile(source_path, src))
    }
}

pub(crate) fn exec_build_cmd(args: Args, help: bool) -> Result<(), DynError> {
    if help {
        write_build_help();
        return Ok(());
    }

    do_exec_build_cmd(args)?;
    process::exit(0)
}
