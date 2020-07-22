use env_logger::Env;
use jl_compiler::rust_api::compile;
use std::{
    env::{self, ArgsOs},
    ffi::OsString,
    fmt::Debug,
    fs,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    process,
};

enum Arg {
    Help,
    Version,
    Build,
}

fn write_help(w: &mut impl Write) -> io::Result<()> {
    write!(
        w,
        r#"{command} v{version}

使用例:
    {command} build main.jacco
    {command} build -

サブコマンド build:
    ソースファイルをビルドします。

    凡例:
        {command} build <FILE|->
    引数:
        <FILE>      ソースファイル
        -           標準入力
    標準出力:
        コンパイル結果のソースコードを標準出力に出力します。

サブコマンド help:
    ヘルプを表示します。

    使用例:
        {command} help

その他:
    -h, --help      ヘルプを表示する
    -V, --version   バージョンを表示する"#,
        command = env!("CARGO_PKG_NAME"),
        version = get_version()
    )
}

fn get_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

fn emit_error(err: &dyn Debug) {
    eprintln!("{:?}", err);
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
        "build" => Ok(Arg::Build),
        subcommand => {
            eprintln!("Unknown subcommand '{}'.", subcommand);
            Err(())
        }
    }
}

fn init_log() {
    let env = Env::default().default_filter_or("trace");
    env_logger::from_env(env).init();
}

fn read_from_stdin(buf: &mut String) -> Result<(), ()> {
    match io::stdin().read_to_string(buf) {
        Ok(_) => Ok(()),
        Err(err) => {
            emit_error(&err);
            Err(())
        }
    }
}

fn read_from_file(path: &Path, buf: &mut String) -> Result<(), ()> {
    match fs::read_to_string(path) {
        Ok(text) => {
            *buf = text;
            Ok(())
        }
        Err(err) => {
            emit_error(&err);
            Err(())
        }
    }
}

fn execute_with_args(mut args: ArgsOs) -> Result<(), ()> {
    args.next();

    match parse_args_for_subcommand(&mut args)? {
        Arg::Help => exit_with_help(),
        Arg::Version => exit_with_version(),
        Arg::Build => {
            let mut src = String::new();
            let output;

            match args.next() {
                Some(arg) if arg.to_str() != Some("-") => {
                    let source_path = PathBuf::from(arg);
                    read_from_file(&source_path, &mut src)?;
                    output = compile(&source_path, &src);
                }
                _ => {
                    let source_path = env::current_dir().unwrap().join("STDIN.jacco");
                    read_from_stdin(&mut src)?;
                    output = compile(&source_path, &src);
                }
            }

            print!("{}", output);
            Ok(())
        }
    }
}

fn main() {
    init_log();

    match execute_with_args(env::args_os()) {
        Ok(()) => {}
        Err(()) => exit_with_help(),
    }
}
