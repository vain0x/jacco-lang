# THE JACCO LANGUAGE

WIP: [TODOs](https://github.com/vain0x/languages/projects/1)

**ジャッコ言語** (`jacco-lang`) は自作プログラミング言語。

## 特徴

当面は「シンプルなC言語」を目指す。(のちのち、ジェネリクスなどの機能を入れて「シンプルな C++」を目指したい。)

- Rust 風の構文: なじみやすく、ほどよく冗長で、高速に解析できる
- C 風の意味論: 相互運用しやすく、枯れていて、オーバーヘッドが小さい

追加要素:

- ~~型安全な分岐: タグつきユニオン・パターンマッチ・網羅性検査~~
- ~~型推論: ローカルのみ~~
- ~~モジュールシステム~~ (未定)

除外要素:

- マクロ
- 条件コンパイル

ツール:

- ~~ビルドツール~~
- ~~パッケージマネージャー~~
- ~~コードフォーマッター~~
- ~~LSP~~
- ~~DAP~~

## 実装状況

(tests ディレクトリを参照。)

## 開発

### 開発: 環境

インストールするもの:

- Git (Windows でシェルスクリプトを動かすのには Git Bash を推奨)
- Rust <https://www.rustlang.org>

ビルドスクリプトなど:

```sh
# ビルド
cargo build

# テストの実行
./test
```

### 開発: テスト

テストランナーは `jl_tests/src/main.rs` 。

`tests/xxx/xxx.jacco` がコンパイルされて `xxx.txt` が生成される。(アサーションは特にない。一部のテストケースでは、C言語として有効でないコードが生成されてしまう。)

## 内部実装

### 内部: ステージ

- 字句列 (token)
    - 字句解析
- 構文木 (parse)
    - 構文解析 (parse)
    - 構文検査 (front::syntax_validation)
    - 名前解決 (front::name_resolution)
    - 命令列の生成 (front::cps_conversion)
        - CPS ノードの構築 (cps::cps_fold)
- CPS 中間表現 (cps)
    - 型推論 (type_resolution)
    - unit 除去 (eliminate_unit)
- C言語 構文木 (clang)
    - 構文木の構築 (clang_gen)
    - 文字列への変換 (clang_dump)

## その他

- [設計のメモ](design.md)
- [略語など](docs/abbreviations.md)
- [その他のメモ](notes.md)
