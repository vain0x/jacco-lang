#![cfg(test)]

use super::cli::compile_v2;
use crate::cli::parse_v2;
use std::{fs, panic, path::PathBuf};

enum Action {
    Pass,
    Parse,
    Compile,
}

enum Expect {
    Missing,
    Parse,
    Run,
    CompileError,
}

impl Expect {
    pub(crate) fn action(&self) -> Action {
        match self {
            Expect::Missing => Action::Pass,
            Expect::Parse => Action::Parse,
            // 実行するところは未実装
            Expect::Run | Expect::CompileError => Action::Compile,
        }
    }
}

enum Actual {
    CompileOk { output: String },
    CompileErr,
    CompilePanic { err: String },
}

#[test]
fn test_features() {
    let mut pass = 0;
    let mut fail = vec![];

    for dir in fs::read_dir(tests_dir().join("features"))
        .unwrap()
        .chain(fs::read_dir(tests_dir().join("edges")).unwrap())
    {
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

        let expect = {
            // TODO: 構文解析して属性を取り出すようにしたい。どこでエラーが起こるべきかも記述できたほうがよい
            if source_code.contains("test(\"compile_error\")") {
                Expect::CompileError
            } else if source_code.contains(r#"#![test("run","#) {
                Expect::Run
            } else if source_code.contains(r#"test("parse")"#) {
                Expect::Parse
            } else {
                Expect::Missing
            }
        };

        let result = match expect.action() {
            Action::Pass => Ok(Some(String::new())),
            Action::Parse => {
                panic::catch_unwind(|| Some(parse_v2(input_file.as_path(), &source_code)))
            }
            Action::Compile => {
                panic::catch_unwind(|| compile_v2(input_file.as_path(), &source_code))
            }
        };

        let actual = match result {
            Ok(Some(output)) => Actual::CompileOk { output },
            Ok(None) => Actual::CompileErr,
            Err(err) => {
                let err = format!("ERROR {:?}", err);
                Actual::CompilePanic { err }
            }
        };

        let (old_pass, fail_len) = (pass, fail.len());
        match (actual, expect) {
            (_, Expect::Missing) => {
                fail.push((input_file, "ファイルの test 属性が見つかりません run, compile_error, parse のどれかが必要です。".to_string()));
            }
            (Actual::CompilePanic { err }, _) => {
                fail.push((input_file, err));
            }
            (Actual::CompileOk { output: actual }, Expect::Parse)
            | (Actual::CompileOk { output: actual }, Expect::Run) => {
                let expected = fs::read_to_string(&output_file).unwrap_or_default();
                if actual != expected {
                    fs::write(&output_file, actual).unwrap();
                }

                pass += 1;
            }
            (Actual::CompileErr, Expect::CompileError) => {
                pass += 1;
            }
            (Actual::CompileOk { output }, Expect::CompileError) => {
                fail.push((
                    input_file,
                    format!(
                        "コンパイルエラーを起こすべきコードのコンパイルが通ってしまいました: {}",
                        output
                    ),
                ));
            }
            (Actual::CompileErr, Expect::Run) => {
                fail.push((input_file, "コンパイルエラー".to_string()));
            }
            (_, Expect::Parse) => unreachable!(),
        }
        assert!(
            pass == old_pass + 1 && (fail_len == fail.len())
                || (pass == old_pass && fail_len < fail.len())
        );
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
