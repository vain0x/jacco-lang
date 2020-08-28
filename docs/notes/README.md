# 設計・実装用のメモ

記述は誤っているかもしれないし、古いかもしれない。

## 言語機能

### 言語: 静的型システム

#### 型の種類

- 整数型:
    - 符号付き: i8, i16, i32, 164, i128, isize
    - 符号なし: u8, u16, u32, u64, u128, usize
- 小数型: fNN (f16〜f128)
- 文字型:
    - c8 (UTF-8 code unit) (C++ の `char8_t` に相当)
    - c16 (UTF-16 code unit)
    - c32 (Unicode scalar value) (Rust の `char` に相当)
- bool
- ポインタ型: `*T`, `*const T`, `*mut T`
- スライス型:
    - TBD: `[T]`?
    - TBD: `str` = `*[c8]`?
- 関数ポインタ型: `fn(T, U) -> V`
- クロージャ型:
    - TBD: `cl(T, U) -> V`?
    - TBD: `cl[S](T, U) -> V`? (S はクロージャの状態の型)
- ユニット型: `()`
    - TBD: `unit` や `void` に変えるかも
- タプル型: `(T, U)`, `(T, U, V)`, ...
- ジェネリック型: `K[T, U]`
- enum (Rust の enum と同様)
- struct
- union

#### 型検査

- 関数のパラメータ、構造体のフィールドなど、export される型は推論されない。
- TBD: 整数リテラルの型
- TBD: シンプルさを目指すのであれば進行定理と保存定理を満たし決定的な (HM より弱い) 型推論として形式化したい。

### 言語: パターン

- 破棄パターン: `_`
- 変数パターン: `x`
- 定数パターン: `TokenKind::Eof` など (`enum TokenKind { Eof, ... }` のような宣言がみえているとき)
- リテラルパターン: `0`, など
- タプルパターン: `(x, y)` など
- タプル構造体パターン: `Point(x, y)` など
- 構造体パターン: `Point { x, .. }` など

### 言語: 式

- リテラル
    - 整数リテラル
        - `42` など。型は iNN, uNN, isize, usize のいずれかに推論される。
    - 小数リテラル
        - `3.14` など。型は fNN のいずれかに推論される。
    - 文字リテラル
        - `'a'` など。整数リテラルと同様、cNN のどれかに推論される。
    - 文字列リテラル:
        - `"hello"`, `r#"wysiwyg"#`, など
    - タプルリテラル: `(x, y)`
    - タプル構造体リテラル: `Point(1, 2)`
    - 構造体リテラル: `Point { x: 1, y: 2 }`
    - バリアントリテラル: `Some(1)`
    - クロージャリテラル: `cl(x) x + 1`, `move cl(x) x.into()`
- パス参照: `f64::PI`
- フィールド参照: `x.m`
- 配列参照: `a[i]`
- 関数呼び出し
    - `f(x, y)`
    - `f::[T](x, y)`
    - `x |> f(y)` (= `f(x, y)`) (参考: [pipe-rewrite operator](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2020/p2011r0.html))
- 単項演算
    - `*p`
    - `&x`, `&const x`, `&mut x`
    - `-x`
    - `!x`
- キャスト
    - `x as f64` など
- 二項演算
    - `x + y` など
- 代入
    - `x = y`
    - `x += y` など
- 条件分岐
    - `if cond { body } else { alt }`
    - `match cond { pat1 => expr1, pat2 => expr2, ... }`
- ループ
    - `while cond { body }`
    - `loop { body }`
    - for (未定)
- ジャンプ
    - break
    - continue
    - return

### 言語: 宣言

- let/const/static
- fn
- extern fn
- struct
- union
- enum
- TBD: extern type?

### 言語: その他

- 属性 (予定)
    - `#[derive(Clone)]`
- 名前空間/モジュール (予定)
    - mod, use
- エラー処理機構
    - `?` operator?
    - panic?
- 抽象化機構:
    - trait?
    - ジェネリクス？
        - `fn f[T]() { ... }`
        - `struct A[T] { ... }`
- リソース制御機構
    - ownership/borrow/lifetime?
    - static contracts?

## パス

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

## 関連記事

読み物系:

- [低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)
- [Next Few Years#Language Design for Locality](https://rust-analyzer.github.io/blog/2020/05/18/next-few-years.html#language-design-for-locality)
    > There’s a very important language property that an IDE can leverage to massively improve performance:
    >
    > *What happens inside a function, stays inside the function*
- [Why the Sorbet typechecker is fast - Made of Bugs](https://blog.nelhage.com/post/why-sorbet-is-fast/)
    - なぜ Sorbet (Ruby の型検査器) の型検査が速いか
- [Reflections on software performance - Made of Bugs](https://blog.nelhage.com/post/reflections-on-performance/)
    - ソフトウェアが速いと何が嬉しいか

資料系:

- [C language - cppreference.com](https://en.cppreference.com/w/c/language)
- [C++ language - cppreference.com](https://en.cppreference.com/w/cpp/language)
- [cpprefjp - C++日本語リファレンス](https://cpprefjp.github.io/)
