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

関係があったりなかったりするリンク集。

言語設計に関する記事:

- [Notes on a smaller Rust](https://boats.gitlab.io/blog/post/notes-on-a-smaller-rust/)
- [Models of Generics and Metaprogramming\: Go, Rust, Swift, D and More - Tristan Hume](https://thume.ca/2019/07/14/a-tour-of-metaprogramming-models-for-generics/)

処理系実装に関する記事:

- [低レイヤを知りたい人のためのCコンパイラ作成入門](https://www.sigbus.info/compilerbook)
- [n月刊ラムダノートにパターンマッチについて寄稿しました | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/11/02/ngekkanramudano_tonipata_nmatchinitsuitekikoushimashita/)
- [Crafting IDE-Ready Compilers - DEV Community 👩‍💻👨‍💻](https://dev.to/cad97/crafting-ide-ready-compilers-500o)
- rust-analyzer の具象構文木の設計: [rust-analyzer/syntax.md at master · rust-analyzer/rust-analyzer](https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/syntax.md)
- [Closures\: Magic Functions | Rusty Yato](https://rustyyato.github.io/rust/syntactic/sugar/2019/01/17/Closures-Magic-Functions.html)

処理系のアーキテクチャに関する記事:

- [Next Few Years#Language Design for Locality](https://rust-analyzer.github.io/blog/2020/05/18/next-few-years.html#language-design-for-locality)
    > There’s a very important language property that an IDE can leverage to massively improve performance:
    >
    > *What happens inside a function, stays inside the function*
- [Why the Sorbet typechecker is fast - Made of Bugs](https://blog.nelhage.com/post/why-sorbet-is-fast/)
    - なぜ Sorbet (Ruby の型検査器) の型検査が速いか
- [Reflections on software performance - Made of Bugs](https://blog.nelhage.com/post/reflections-on-performance/)
    - ソフトウェアが速いと何が嬉しいか
- [Three Architectures for a Responsive IDE](https://rust-analyzer.github.io/blog/2020/07/20/three-architectures-for-responsive-ide.html)
- [A Few More Reasons Rust Compiles Slowly | PingCAP](https://pingcap.com/blog/reasons-rust-compiles-slowly)
- [Introducing MIR | Rust Blog](https://blog.rust-lang.org/2016/04/19/MIR.html)
- Swift のジェネリクスの実装に関する話: [言語処理系勉強会に参加してきた | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2018/11/17/gengoshorikeibenkyoukainisankashitekita/)

資料系:

- [C language - cppreference.com](https://en.cppreference.com/w/c/language)
- [C++ language - cppreference.com](https://en.cppreference.com/w/cpp/language)
- [cpprefjp - C++日本語リファレンス](https://cpprefjp.github.io/)
- [Zen Language Documentation](https://zen-lang.org/ja-JP/docs/)

その他の記事:

- [How to C (as of 2016)](https://matt.sh/howto-c)
- [String interners in Rust - DEV Community 👩‍💻👨‍💻](https://dev.to/cad97/string-interners-in-rust-797)
- C# の配列のメモリプール: [(C#) ArrayPool<T>.Shared 解体新書 - ネコのために鐘は鳴る](https://ikorin2.hatenablog.jp/entry/2020/07/25/113904)
- Kotlin の expect/actual 機能: [Connect to platform-specific APIs - Kotlin Programming Language](https://kotlinlang.org/docs/reference/mpp-connect-to-apis.html)
- [Solving the structured control flow problem once and for all | by Yuri Iozzelli | leaningtech | Medium](https://medium.com/leaningtech/solving-the-structured-control-flow-problem-once-and-for-all-5123117b1ee2)
- [A Note on BuckleScript's New Syntax and Its Future Support Commitments | Reason Blog](https://rescript-lang.org/blog/a-note-on-bucklescripts-future-commitments)
- [なぜ default export を使うべきではないのか？ - LINE ENGINEERING](https://engineering.linecorp.com/ja/blog/you-dont-need-default-export/): default export は自動 import 機能と相性が悪いという話。言語設計は自動 import を念頭に置くべきかもしれない
- [単一メンバunionの使い道 - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20200331/p1)
- [メモリアロケーションに対する罪悪感 - kawasin73のブログ](https://kawasin73.hatenablog.com/entry/2019/11/10/112301)
- [Reference-counting garbage collection can be quite efficient - Rust Internals](https://internals.rust-lang.org/t/reference-counting-garbage-collection-can-be-quite-efficient/10898)
- <https://twitter.com/elpin1al/status/1072101590117449728>
- [セルフホストCコンパイラaqcc 開発記 | カオスの坩堝](https://anqou.net/poc/2018/08/21/post-1853/)
- [プログラミング言語を作る](http://kmaebashi.com/programmer/devlang/index.html)

書籍:

- [電子通信情報系コアテキストシリーズ C-1 実践コンパイラ構成法 | コロナ社](https://www.coronasha.co.jp/np/isbn/9784339019339/) ([正誤表](https://www.ed.tus.ac.jp/j-mune/ccp/))
- [プログラミング言語の基礎概念(ライブラリ情報学コア・テキスト24)](https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/)
- ~~[プログラム意味論の基礎](https://www.saiensu.co.jp/search/?isbn=978-4-7819-1483-1&y=2020)~~ まだ読んでない
- [最新コンパイラ構成技法](https://www.seshop.com/product/detail/11456) (通称タイガー本) (あんまり理解してない)
- [型システム入門 プログラミング言語と型の理論 | Ohmsha](https://www.ohmsha.co.jp/book/9784274069116/) (通称 TaPL)
- Compilation with Continuations

書評:

- [お薦めのコンパイラの本とか | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/02/16/osusumenokonpairanohontoka/)

特に関係のない宣伝:

- [プログラミング言語処理系が好きな人の集まり](https://prog-lang-sys-ja-slack.github.io/wiki/)
