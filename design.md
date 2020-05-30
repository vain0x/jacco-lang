# 設計メモ

Jacco 言語の設計判断に関するメモ。

## 構文

C言語の構文には問題がある (`#include`、マクロ、無限先読み、`t*p` 問題、ぶら下がり if、関数ポインタ型など)。Rust の構文はC言語に似ていつつも、多くの問題を解決している。そのため、ジャッコ言語は Rust の構文をベースにする。

おそらく ergonomics や既存言語との類似性を目的として、Rust にも解析を難しくする要素がある。ジャッコ言語は ergonomics より工学的なシンプルさを目指すため、これらの要素を変更して、context-free にする。(予定)

- Rust は構造体リテラル `K {}` と制御構文 `if cond {}` などの構文が衝突していて、一部の式には構造体リテラルが出現できないことにして対処している。
    - 制御構文の条件式をカッコを必須として `if (cond) {}` にする。
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
    - 構造体リテラル: `Point { x: 1, y: 2 }`
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
    - `!x`
    - `x as f64`
- 二項演算
    - `x + y` など
- 代入
    - `x = y`
    - `x += y` など
- 条件分岐
    - `if (cond) { body } else { alt }`
    - match
- ループ
    - `while (cond) { body }`
    - `loop { body }`
    - for
- ジャンプ
    - break
    - continue
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