# メモ

記述は誤っているかもしれないし、古いかもしれない。

## モチベーションやコンセプトや目標など

- 作者の趣味
- Rust の練習
- CPS 中間表現の練習
- 既存の言語に似せること
    - C, C#, Rust, JavaScript などに「似ている」と感じられる範囲に保ちたい
- 既存の言語から乖離しない範囲で、言語仕様・実装を小さく保つ工夫をすること
    - メリットが大きくて実装がそれほど難しくない機能は冗長でも実装してもいい
- 言語機能の試験実装のベースになる処理系を作ること
    - jacco 処理系をフォークして何か言語機能 (GC とか) を足す、などの作業が気軽にできるようにしたい
- コンパイルが高速であること
- LSP を備えること
- DAP を備えること

## 実行例

```sh
cargo build && RUST_LOG=error $J build ./tests/while/while.jacco
```

## 字句解析

文字列として与えられたソースコードをトークンの列にする。トークンはソースコード上に出現する「分割不能な単位」であり、主に以下の種類がある。

- EOF: 入力の終端
    - 必須ではないが、構文解析時の番兵として便利
- トリビア: 構文的に意味を持たない部分
    - shebang (`#!/bin/sh`)
    - 空白 (\x20, \t)
    - 改行 (\n, \r\n)
    - コメント (`// ...` や `/* ... */`)
    - 解釈不能 (ヌル文字、ソースコードの文字列エンコーディングに違反している部分、ソースコードの地の文に出現しえない文字など)
        - BOM
- リテラル:
    - 数値リテラル: 42 とか `0.314e+1` とか
    - 文字リテラル: `'a'` など
    - 文字列リテラル: `"hello"` など
- 識別子:
    - キーワードや予約語である識別子
        - `fn` など
    - キーワードでない識別子
        - `f` など
- 記号
    - `+` や `+=` など

字句解析の目的は文字列をいくつかの区間に分割し、それぞれの区間のトークンとしての種類を特定すること。

例:

```
    fn main() {}

^^^^ 空白
    ^^ キーワード(fn)
      ^ 空白
```

```
(続き)
    fn main() {}
       ^^^^ 識別子
           ^ 左丸カッコ
            ^ 右丸カッコ
             ^ 空白
```

```
(続き)
    fn main() {}
              ^ 左波カッコ
               ^ 右波カッコ
                | EOF
```

## CPS 変換

CPS 変換の方法。

関連記事:

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)
- [RustでCPS変換が簡単になったよという話 | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/12/07/rustdecpshenkangakantanninattayotoiuhanashi/)

### CPS ノード

凡例

```
    (ノードの種類 [引数列...] [結果列...] [継続列...])
```

x, y を加算して、結果を z に束縛し、継続 n を行う (+) 命令のノード:

```
    (+ [x y] [z] [n])
```

x, y が等しければ継続 eq, そうでなければ継続 ne を行う (=) 命令のノード:

```
    (= [x y] [] [eq ne])
```

### 例1: 加算のみ

```rust
fn f() {
    40 + 2
}
```

アセンブリ風のフラットな命令列に変換する。 `+:1` の 1 は継続の個数。

```
x <- +:1(40, 2)
jump:0(return, x)
```

プリミティブ `+:1` は1つの継続を持つので、後続の命令列からノードを1つ構築する。ノードの構築処理を再帰的に呼ぶ。
jump:0 は継続を持たないので、単独でノードになる。これが再帰呼び出しから返り、`+:1` の継続になる。

```
(+ [40 2] [x] [
    (jump [x])
])
```

### 例2: 入れ子の式

```rust
fn f() {
    (2 + 3) * 4
}
```

命令列に変換する。

```
x <- +:1(2, 3)
y <- *:1(x, 4)
jump:0(return, y)
```

`+:1` → `*:1` → `jump:0` の順で再帰的にノードが構築される。

```
(+ [2 3] [x] [
    (* [x 4] [y] [
        (jump y)
    ])
])
```

### 例3: 分岐

```rust
fn f() {
    (if a > 0 {
        1
    } else {
        0
    }) * 2
}
```

if の後続の処理は (`... * 2`) は then/else 節の両方から参照されるので、関数としてまとめる。(そうしないとコード量が爆発してしまう。)

```
    p <- >:1(a, 0)
    if:2(p)

    // then
    jump:0(endif, 1)

    // else
    jump:0(endif, 0)

endif(x):
    y <- *:1(x, 2)
    jump:0(return, y)
```

```
    (fix [
        (label endif [x] [
            (* [x 4] [y] [
                (jump [return y])
            ])
        ])
    ] [
        (> [a 0] [p] [
            (if [p] [] [
                // then
                (jump [endif 1])
                // else
                (jump [endif 0])
            ])
        ])
    ])
```

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
