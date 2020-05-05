# THE JACCO LANGUAGE

WIP

**ジャッコ言語** (`jacco-lang`) は自作プログラミング言語。

## 特徴

当面は「シンプルなC言語」を目指す。(のちのち、ジェネリクスなどの機能を入れて「シンプルな C++」を目指したい。)

- Rust 風の構文
- C 風の意味論
- C ソースコードへのコンパイル

## 構文

C言語の構文には問題がある (`#include`、マクロ、無限先読み、`t*p` 問題、ぶら下がり if、関数ポインタ型など)。Rust の構文はC言語に似ていつつも、多くの問題を解決している。そのため、ジャッコ言語は Rust の構文をベースにする。

おそらく ergonomics や既存言語との類似性を目的として、Rust にも解析を難しくする要素がある。ジャッコ言語は ergonomics より工学的なシンプルさを目指すため、これらの要素を変更して、context-free にする。(予定)

- Rust は構造体リテラル `K {}` と制御構文 `if cond {}` などの構文が衝突していて、一部の式には構造体リテラルが出現できないことにして対処している。
    - ジャッコ言語は構造体リテラルを `K::{}` にする。(あるいは、制御構文の条件式をカッコを必須として `if (cond) {}` にする。)
- Rust は型引数のカッコに `<>` を使っていて、比較演算子やシフト演算子と弁別しづらい。
    - ジャッコ言語は `[]`/`::[]` を使う。
- Rust はブロック文の直後のセミコロンを省略するために、打ち切り規則を使っている (参考: [Rustの文でセミコロンを省略してよい条件](https://qnighy.hatenablog.com/entry/2017/04/22/070000))。
    - ジャッコ言語は構文規則を調整して対処する。(予定)
- Rust は強力なマクロ機能を持っていて、マクロを利用している部分は静的解析がしづらい。
    - ジャッコ言語はマクロを持たない。

## 言語機能

### 言語: 型

- 整数型: iNN (i8〜i64), uNN (u8〜u64), isize, usize
- 小数型: f32, f64
- 文字型:
    - c8 (UTF-8 code unit)
    - c32 (Unicode scalar value)
- ポインタ型: `*T`
- タプル型: `()`, `(T, U)`
- 関数ポインタ型: `fn(T, U) -> V`
- ジェネリック型: `K[T, U]`
- struct
- union
- enum (tagged union)

### 言語: パターン

- 破棄パターン: `_`
- 変数パターン: `x`
- リテラルパターン: `0`, `Point(x, y)`, など

### 言語: 式

- リテラル
    - 整数リテラル
    - 小数リテラル
    - 文字リテラル
        - `b'a': c8`
        - `'a': c32`
    - 文字列リテラル: `"hello"`, `r#"wysiwyg"#`, など
    - タプルリテラル: `(x, y)`
    - 構造体リテラル: `Point::{ x: 1, y: 2 }`
    - タプル構造体リテラル: `Point(1, 2)`
    - バリアントリテラル: `Some(1)`
    - クロージャリテラル: `|x| x + 1`, `move || x.into()`
- パス参照: `f64::PI`
- フィールド参照: `x.m`
- 配列参照: `a[i]`
- 関数呼び出し
    - `f(x, y)`
    - `f::[T](x, y)`
    - `x |> f(y)` (= `f(x, y)`) (参考: [pipe-rewrite operator](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p2011r0.html))
- 単項演算
    - `*p`
    - `&x`
    - `-x`
    - `x as f64`
- 二項演算
    - `x + y` など
- 代入
    - `x = y;`
    - `x += y;` など
- if/match
- while/for/loop
- break/continue
- return
- (TODO: goto/label?)

### 言語: 宣言

- let/const/static
- fn
- extern fn
- struct
- union
- enum

### 言語: その他

- 属性 (予定)
    - `#[derive(Clone)]`
- 名前空間/モジュール (予定)
    - mod, use
- ジェネリクス (予定)
    - `fn f[T]()`

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

### 内部: 略語

| 略語   | 原語           | 意味 |
|:------|:--------------|:--|
| alt   | alternative   | 代替 |
| arg   | argument      | 実引数 |
| attr  | attribute     | 属性 |
| cal   | callee        | 呼び出される関数 |
| cont  | continuation  | 継続 |
| cond  | condition     | 条件 |
| decl  | declaration   | 宣言 |
| expr  | expression    | 式 |
| fn    | function      | 関数 |
| gen   | generation    | 生成 |
| ident | identifier    | 識別子 |
| lit   | literal       | リテラル |
| mod   | module        | モジュール |
| mod   | modulo        | 剰余 |
| op    | operation     | 演算 |
| op    | operator      | 演算子 |
| opt   | optional      | 省略可能 |
| param | parameter     | 仮引数 |
| prim  | primitive     | プリミティブ |
| semi  | semicolon     | セミコロン `;` |
| ty    | type          | 型 |

### 内部: 頭字語

- continuation passing style (CPS): 継続渡し形式

### 内部: ステージ

- 字句
    - 字句解析
- 構文木
    - 構文解析
    - 名前解決
- CPS 中間表現
    - 命令列の生成 (cps_gen)
    - CPS ノードの構築 (cps_fold)
- C言語
    - 構文木の構築 (clang_gen)
    - 文字列への変換 (clang_dump)
