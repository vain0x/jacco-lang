# THE JACCO LANGUAGE

WIP: [TODOs](https://github.com/vain0x/languages/projects/1)

**ジャッコ言語** (`jacco-lang`) は自作プログラミング言語。

## 特徴

当面は「シンプルなC言語」を目指す。(のちのち、ジェネリクスなどの機能を入れて「シンプルな C++」を目指したい。)

- Rust 風の構文
- C 風の意味論

## 実装状況

特になし。(tests ディレクトリを参照。)

## 開発

### 開発: 環境

- Rust <https://www.rustlang.org> のツールチェインをインストールする。

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

### 内部: 用語集

略語表:

| 略語   | 原語           | 意味 |
|:------|:--------------|:--|
| alt   | alternative   | 代替 |
| arg   | argument      | 実引数 |
| attr  | attribute     | 属性 |
| cont  | continuation  | 継続 |
| cond  | condition     | 条件 |
| decl  | declaration   | 宣言 |
| expr  | expression    | 式 |
| gen   | generation    | 生成 |
| ident | identifier    | 識別子 |
| init  | initialization| 初期化 |
| lit   | literal       | リテラル |
| mod   | module        | モジュール |
| op    | operation     | 演算 |
| op    | operator      | 演算子 |
| opt   | optional      | 省略可能 |
| param | parameter     | 仮引数 |
| prim  | primitive     | プリミティブ |
| semi  | semicolon     | セミコロン |
| sig   | signature     | シグネチャ |
| ty    | type          | 型 |

- Gx, Tx など
    - 何らかの処理で使うデータを入れた構造体。なんとか context の略。
- continuation passing style (CPS): 継続渡し形式

### 内部: プロジェクト構成

依存関係は一方向: 下流のプロジェクトは上流のプロジェクトを参照できて、逆はできない。

- jl_cps_repr: CPS 中間表現
- jl_cps_actions: CPS 中間表現の解析・変換処理。型推論、最適化など
- jl_syntax: ジャッコ構文。字句解析、構文解析、名前解決など
- jl_clang: C言語バックエンド。CPS 中間表現からC言語のソースコードを生成する
- jl_tests: 自動テスト
- ~~jl_lsp~~: LSPサーバー (未実装)
- ~~jacco_cli~~: CLIアプリ (未実装)
- ~~jacco_lib~~: ライブラリ (未実装)

### 内部: ステージ

- 字句 (token)
    - 字句解析
- 構文木 (parse)
    - 構文解析
    - 名前解決 (name_resolution)
    - 命令列の生成 (cps_gen)
- CPS 中間表現 (cps)
    - CPS ノードの構築 (cps_fold)
    - 型推論 (type_resolution)
    - ユニット除去 (eliminate_unit)
- C言語 (clang)
    - 構文木の構築 (clang_gen)
    - 文字列への変換 (clang_dump)

## その他

- [設計のメモ](design.md)
- [その他のメモ](notes.md)
