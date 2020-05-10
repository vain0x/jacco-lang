//! テストランナー
//!
//! USAGE: `jl_tests <ソースファイルのパス>`
//!
//! `foo.jacco` をコンパイルした結果を `foo.txt` に出力する。

use jl_compiler::compile;
use std::env;
use std::fs;
use std::path::PathBuf;

fn init_log() {
    let env = env_logger::Env::default().default_filter_or("trace");
    env_logger::from_env(env).init();
}

fn main() {
    init_log();

    let mut args = env::args_os();
    args.next();

    let first_arg = args.next().expect("expected a file path");
    let source_path = PathBuf::from(first_arg);
    let source_code = fs::read_to_string(&source_path).unwrap();

    let output = compile(&source_path, &source_code);

    let out_path = source_path.with_extension("txt");
    fs::write(&out_path, &output).unwrap();
}
