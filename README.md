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

#### 略語など

| 略語   | 原語           | 意味 |
|:------|:--------------|:----|
| alt   | alternative   | 代替 (else の式) |
| arg   | argument      | 実引数 |
| attr  | attribute     | 属性 |
| cond  | condition     | 条件 |
| cont  | continuation  | 継続 |
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
| ty    | type          | (静的) 型 |

- Gx, Tx など
    - 何らかの処理で使うデータを入れた構造体。なんとか context の略。
- outline は関数の引数の型などの外部に公開されるインターフェイスを指すために使っている。(一般的な用法ではない)
- continuation passing style (CPS): 継続渡し形式

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
- [その他のメモ](notes.md)
