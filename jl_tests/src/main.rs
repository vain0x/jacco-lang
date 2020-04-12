use jl_compiler::compile;
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let mut args = env::args_os();
    args.next();

    let first_arg = args.next().expect("expected a file path");
    let source_path = PathBuf::from(first_arg);
    let source_code = fs::read_to_string(&source_path).unwrap();

    let output = compile(&source_path, &source_code);

    let out_path = source_path.with_extension("txt");
    fs::write(&out_path, &output).unwrap();
}
