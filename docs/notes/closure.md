# クロージャ

WIP

匿名の関数を式中で宣言、生成するための機能を、ここではクロージャと呼ぶことにする。(まだ変数のキャプチャはできない。)

## クロージャの構文

```rust
    fn(params..) expr
```

または (結果型を注釈するとき):

```rust
    fn(params) -> ty { stmts...; expr }
```

自由変数を含むことはできない。

## クロージャの型検査

関数文と違ってパラメータや結果の型注釈が必須でないので、上から下に型推論できない。

クロージャが出現したとき、型注釈は解決するが、本体の型検査はまだ行わない。クロージャの式には矢印型をつけ、型注釈のない部分はフレッシュな型変数で埋める。この矢印型と型変数からクロージャを引けるようにしておく。

この矢印型が以下のいずれかのタイミングではじめて出現したとき、その出現に対する型検査を行うことでクロージャの型が判明する。このタイミングでクロージャの本体の型検査を行う。

- 関数呼び出しされるとき (`(fn...)()`)
- 「型注釈のないクロージャの引数」ではない引数に渡されるとき (`g((fn...), ...)`)
- as の左辺になるとき (`(fn...) as T`)
- 型注釈を持つ let に束縛されるとき (`let f: fn(i32) -> i32 = fn...`)
- 代入式の右辺になるとき (`*p = fn...`)
- 関数から return されるとき (`fn f() { fn... }`, `fn f() { return fn... }`)

クロージャの本体の型検査を行うことなく型検査が終了した場合、型注釈のない部分をすべて never で埋めて、本体の型検査を行う。

TODO: 型検査と名前解決を同時に行う場合、名前解決を遅延するためにクロージャが出現した瞬間の環境を記録する必要がある？ (キャプチャしないので、パラメータや let で宣言されたローカル変数は見れないが、const/static などは見れる。)

## クロージャのコード生成

いまのところクロージャは状態を持たないので、コード生成では単に関数文に置き換えるだけでいい。

```rust
    fn(params) expr
    //=> fn(params) -> ty { expr }

    fn(params) -> ty block
    //=> fn f(params) -> ty { block }; block
```

## 今後の拡張の予定

- 状態を持つクロージャ
- ジェネリックなクロージャ
- 複数の「メソッド」を持つクロージャ

ほか

## 設計のメモ

構文:

- アロー関数の構文 (`(params) => expr`) は構文解析上の問題がある。`(params)` がカッコ式ではなくパラメータリストであると判定するには無制限に先読みして `=>` をみる必要がある。
- Rust の `|params| expr` 構文にする？
- PHP の `fn(params) => expr` 構文にする？

意味論:

- クロージャが状態を持ち、さらにローカル変数へのポインタを持つ場合、クロージャを関数から返したり配列に格納したりするポインタが無効化してしまう。これは構文的に気づきづらい。
- クロージャが状態を持つ場合、単一の状態に対して複数の操作や再帰的な操作を提供したいことがしばしばある。

## 関連記事

Rust のクロージャは呼び出し時に発生する所有権の移動や状態の変化を型システムにエンコードしている:

- [Closures - Rust By Example](https://doc.rust-lang.org/rust-by-example/fn/closures.html)
- [Advanced Functions and Closures - The Rust Programming Language](https://doc.rust-lang.org/book/ch19-05-advanced-functions-and-closures.html)
- [Closures\: Magic Functions | Rusty Yato](https://rustyyato.github.io/rust/syntactic/sugar/2019/01/17/Closures-Magic-Functions.html)
- [rfcs/0114-closures.md at master · rust-lang/rfcs](https://github.com/rust-lang/rfcs/blob/master/text/0114-closures.md)
    - [rfcs/0968-closure-return-type-syntax.md at master · rust-lang/rfcs](https://github.com/rust-lang/rfcs/blob/master/text/0968-closure-return-type-syntax.md)
    - [rfcs/1558-closure-to-fn-coercion.md at master · rust-lang/rfcs](https://github.com/rust-lang/rfcs/blob/master/text/1558-closure-to-fn-coercion.md)

Swift はエスケープする変数やクロージャを `@escaping` でマークすることを要求している:

- [Closures — The Swift Programming Language (Swift 5.3)](https://docs.swift.org/swift-book/LanguageGuide/Closures.html)

Kotlin は inline クロージャにより non-local return をサポートしている:

- [Inline Functions and Reified Type Parameters - Kotlin Programming Language](https://kotlinlang.org/docs/reference/inline-functions.html)

C++ は明示的なキャプチャリストやジェネリックなクロージャをサポートしている:

- [ラムダ式 - cpprefjp C++日本語リファレンス](https://cpprefjp.github.io/lang/cpp11/lambda_expressions.html)

PHP は明示的なキャプチャリストをサポートしていて、おそらくジャッコ言語に明示的なキャプチャリストを採用するときは同様の構文にする:

- [PHP\: 無名関数 - Manual](https://www.php.net/manual/ja/functions.anonymous.php)
