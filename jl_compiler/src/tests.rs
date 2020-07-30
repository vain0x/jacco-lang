#![cfg(test)]

use super::cli::compile_v2;
use std::{fs, panic, path::PathBuf};

#[test]
fn test_features() {
    let mut pass = 0;
    let mut fail = vec![];

    for dir in fs::read_dir(tests_dir().join("features")).unwrap() {
        let dir = match dir {
            Ok(it) => it,
            Err(_) => continue,
        };

        let (input_file, output_file) = {
            let file_name = dir.file_name();
            let name = file_name.to_string_lossy();
            let input_file = dir.path().join(format!("{}.jacco", name));
            let output_file = dir.path().join(format!("{}.txt", name));
            (input_file, output_file)
        };
        let source_code = fs::read_to_string(&input_file)
            .unwrap_or_else(|err| panic!("expected {:?} ({:?})", input_file, err));

        let result = panic::catch_unwind(|| compile_v2(input_file.as_path(), &source_code));
        let actual = match result {
            Ok(Some(it)) => it,
            Ok(None) => {
                fail.push((input_file, "コンパイルエラー".to_string()));
                continue;
            }
            Err(err) => {
                fail.push((input_file, format!("ERROR {:?}", err)));
                continue;
            }
        };

        let expected = fs::read_to_string(&output_file).unwrap_or_default();
        if actual != expected {
            fs::write(&output_file, actual).unwrap();
        }

        pass += 1;
    }

    if !fail.is_empty() {
        panic!("fails {:#?}", fail)
    }
    if pass == 0 {
        panic!("no tests");
    }
}

fn tests_dir() -> PathBuf {
    let manifest_dir: &'static str = env!("CARGO_MANIFEST_DIR");
    let tests_dir = PathBuf::from(manifest_dir)
        .join("../tests")
        .canonicalize()
        .unwrap();
    assert_eq!(tests_dir.file_name().unwrap().to_str(), Some("tests"));
    tests_dir
}
