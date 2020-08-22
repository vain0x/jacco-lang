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

## 構文解析

ソースコードを字句解析してトークン列を得られたとする。これを構文解析して構文木を得たい。

トリビア (空白やコメントなど) は構文規則的に意味がないのでトークン列から取り除く。

構文解析の目的はトークン列を木構造に変形し、それぞれのノードの種類を特定すること。

### 構文木の組み立て

式文の例:

```
入力:
    2 + 3 * 5 + 7;

トークン列:
    2, '+', 3, '*' 5, '+' 7, ';', EOF

構文木:
    [root
        [expr_decl
            [binary
                [binary
                    [lit 2]
                    +
                    [binary
                        [lit 3]
                        *
                        [lit 5]
                    ]]
                +
                [lit 7]]
            ;]]

工程:

    parse_root
        start_element
        parse_decl
            start_element
            parse_expr
                parse_add
                    parse_mul
                        parse_atom
                            start_element
                            bump 2
                            finish(lit)

                    start_parent
                    bump +

                    parse_mul
                        parse_atom
                            bump 3
                            finish(lit)
                        start_parent
                        bump *
                        parse_atom
                            start_element
                            bump 5
                            finish(lit)
                        finish(binary)

                    start_parent
                    bump +

                    parse_mul
                        parse_atom
                            start_element
                            bump 7
                            finish(lit)
                    finish(binary)
                finish(binary)
            bump ;
            finish(expr_decl)
        finish(root)
```

## 名前解決

TBD

## 型解決 (型検査・型推論)

規則に従って式や項の型を計算し、検査する処理を型解決と呼んでいる。

目的:

- 型つけ規則に違反する式を発見し、コンパイルエラーを報告する。
- ローカル変数などの型注釈を持たないシンボルの型を決定する。

前提: エイリアスとローカル変数以外のシンボルの型はすべて型注釈が必須であり、型を推論する必要はない。
(関数のパラメータや結果の型、構造体のフィールドの型など。クロージャを導入したらこの前提は崩れる。)

戦略: 基本的には、式を評価順にみていき、それぞれの式の型を貪欲に決めていく。

### シンボルの宣言の型

fn 宣言などにより定義される、シンボル本来の型を宣言の型と呼ぶ。
例えば `extern fn abort() -> never` の宣言の型は `fn() -> never` という矢印型になる。

宣言の型は式の型とは異なる可能性がある。
例えば後述の never 型に関する規則のため、`abort() as i32` における `abort` の「式の型」は `fn() -> i32` になる。

### 具体的な型

以下の型は具体的な型と呼ぶ。

- never, unit, bool
- `*unknown`, `*mut unknown`
- 数値型 (iNN, uNN, fNN, cNN)
- enum, struct で定義されるシンボルの型
- 具体的な型へのポインタ型 (`*i32`, `*mut i32` など)
- 具体的な型からなる矢印型 (`fn() -> i32` など)
- 具体的な型に解決されたメタ型変数

unknown と、具体的な型に解決されていないメタ型変数は具体的な型ではない。

### 型の制約

- 同一性の制約: ある2つの式が同じ型を持つこと。
    例えば `if { x } else { y }` の `x`, `y` は同じ型でなければいけない。
- キャスト可能性の制約: 式の型がある型へキャスト可能であること。
    例えば `x as i32` の式 x は、型 i32 へのキャストが可能な型を持たなければいけない。
- その他の制約: ある式がいくつかの種類の型のどれかを持つこと。
    例えば `x + y` のとき x はポインタ型か数値型でなければいけない。

### 単一化

TBD

### never に関する規則

ある式の型が T に解決されたとき、T 自身または T の共変の位置に出現する never をそれぞれフレッシュな型変数で置き換える。
(never <: T の部分型つけをシミュレートするため。)

これはすべての式の型解決後に適用されるが、実際にはシンボルの宣言の型と、関数呼び出しの結果型、ジャンプ式の型にしかそのような never は出現しないはず。

#### 例1: 結果型が never な関数の呼び出しの型つけ

```rust
fn yes() -> never {
    loop {}
}

pub fn main() -> i32 {
    no_return()
}
```

yes の宣言の型は `fn() -> never` なので、式 `yes()` における yes にはこの型がつく。
never が共変の位置に出現しているので、フレッシュな型変数 T を導入して、`yes: fn() -> T` となる。
単一化により T = i32 となり、型検査が通る。
最終的に `yes: fn() -> i32` になっていることに注意。(宣言の型と異なる。実行時に問題は起きない。)

#### 例2: 「結果型が never である関数」を受け取る関数の呼び出しの型付け

(ジャッコ言語の構文的に関数に関数を渡すことはできないが、中間表現ではこのような状況が発生する。)

```rust
fn jump(k: fn() -> never) -> never {
    k()
}

fn one() -> i32 {
    1
}

pub fn main() -> i32 {
    let _ = jump(yes); // OK
    let _ = jump(one); // NG
    0
}
```

式 `jump(yes)` における式 jump, yes の型は以下の通り
(T, U はそれぞれフレッシュな型変数):

- jump: `fn(fn() -> never) -> T`
- yes: `fn() -> U`

なお jump の引数 k の結果型である never は、共変の位置ではないので型変数に置き換えられない。
U = never と単一化して、型検査が通る。

一方、式 `jump(one)` は型検査を通らない。この式における式 jump, one の型は以下の通り:

- jump: `fn(fn() -> never) -> T`
- one: `fn() -> i32`

never != i32 なので単一化に失敗する。
実際、この呼び出しができてしまうと、結果が never なはずの jump が return してしまう。
すなわち、共変でない位置に出現する never を型変数に置き換えると、不正なコードが型検査を通ってしまう。

### unknown に関する規則

ある式の型が T に解決されたとき、T の反変の位置に出現する unknown をそれぞれフレッシュな型変数で置き換える。
(T <: unknown の部分型つけをシミュレートするため。)

never と同様。

```rust
extern fn malloc() -> *mut unknown;
extern fn free(p: *mut unknown);

pub fn main() -> i32 {
    let p = malloc(8) as *mut i32;
    free(p);
    0
}
```

`malloc(8)` における式の型は `fn() -> *mut unknown` である。
(unknown は不変の位置に出現しているので型変数に置換されない。)

`free(p)` における free の式の型は `fn(p: *T)` になり、T = i32 に単一化されて型検査を通る。
なお p の型に mut がついていないとき、`*mut T` に `*i32` は単一化できないので、型検査が通らない。

### リテラル

unit は unit 型、true, false は bool 型。

数値リテラル、文字リテラル、文字列リテラルの型は、型接尾辞 (`42_i32` みたいなの) がついているときは、その型になる。

そうでなければ、構文的に確定しない。以下の制約を持つ:

- 数値リテラルの型は数値型 (iNN, uNN, fNN, cNN のいずれか) でなければいけない。
- 小数部か指数部を持つ数値リテラルの型は浮動小数点数型 (fNN) でなければいけない。
- 文字リテラルの型は文字型 (cNN) でなければいけない。
- 文字列リテラルの型は文字型への読み取り専用ポインタ (`*cNN`) でなければいけない (?)
    - FIXME: 可能なら `Str` などの構造体に推論する規則のほうがよい (?)

直接の親となる式や宣言からリテラルの型が決まらないとき、リテラルの型は解決できない。

### 識別子・パス

シンボルに解決された識別子やパスの式には、そのシンボルの宣言の型がつく。

### レコード式

`T { f1: x1, ... }`

- フィールドに割り当てる式の型は、そのフィールドと同じ型でなければいけない。
- 結果は T になる。

### フィールド式

`x.a`

x は名前が a であるフィールドを持つレコードの型でなければいけない。
結果はそのフィールドの宣言の型になる。

### 関数呼び出し

`f(x1, y1, ...)`

- f は矢印型でなければいけない。
- 引数は、対応するパラメータと同じ型でなければいけない。
- 結果は関数の結果型になる。

### 添字式

`x[i]` は `*(x + i)` と同じ規則に従う。

### キャスト

`x as T` は以下のルールに従う。

- T が具体的な型でないとき:
    - x の型は T と同じでなければいけない。
- T が具体的な型のとき:
    - x の型が未束縛のメタ型変数なら、T を束縛する。
    - x の型は T にキャスト可能な型でなければいけない。

キャスト可能の条件:

- 任意の型 T は T 自身にキャスト可能 (反射性)
- never は T にキャスト可能
- 型 T は unit にキャスト可能
- bool, iNN, uNN, cNN, fNN はすべて相互にキャスト可能
- ポインタ型とポインタサイズの整数型はすべて相互にキャスト可能
- const enum 型は iNN, uNN にキャスト可能

### 単項演算

- `!x`: x は bool 型、結果も bool 型。
- `-x`: x は整数型でなければいけない。結果は同じ型になる。
- `*p`: p はポインタ型 `*T` または `*mut T` でなければいけない。結果は T になる。
- `&x`: x の型を T とするとき、結果は `*T` になる。x は可変な式でなければいけない。
- `&mut x`: x の型を T とするとき、結果は `*mut T` になる。

### 二項演算

- `x + y`, `x - y`:
    - x がポインタ型なら、y は整数型でなければいけない。結果は x の型になる。
    - そうでなければ、x, y は同じ型で、さらに数値型でなければいけない。結果はその型になる。
- `x * y`, `/`, `%`:
    - 左右が同じ型で、さらに数値型でなければいけない。結果はその型になる。
- ビット演算:
    - 左右が同じ型で、fNN ではない数値型でなければいけない。結果はその型になる。
- `==`, `!=`:
    - 左右が同じ型で、同値比較が可能な型でなければいけない。結果は bool になる。
- 順序の比較 (`<` など):
    - 左右が同じ型で、順序比較が可能な型でなければいけない。結果は i32 になる。
- 論理演算
    - `x && y`: `if x { y } else { false }` と同じ規則に従う。
    - `x || y`: `if x { true } else { y }` と同じ規則に従う。

同値比較が可能な型の条件:

- bool, 数値型, const enum
- (TODO: 同値比較が可能な型からなるレコード？)

順序比較が可能な型の条件:

- bool, 数値型, const enum
- (TODO: 順序比較が可能な型からなるレコード？)

### 代入

- `x = y`: x, y は同じ型でなければいけない。
- `x += y` など: 二項演算と同様

### ブロック

`{ d1; d2; ...; x }`

- 宣言が含まれていないか、最後の宣言が式宣言でないとき:
    - 結果は unit である。
- 最後の宣言が式宣言のとき:
    - 結果はその式の結果型と同じ。

### ジャンプ

break, continue, return の結果は never 型になる。
(never に関する規則によりフレッシュな型変数に置き換わる。)

- break: 引数は、対応するループの結果と同じ型でなければいけない。
- return: 引数は、対応する関数の結果型と同じ型でなければいけない。

### 分岐: if

`if cond { x } else { y }` は `match cond { true => x, false => y }` と同じ規則に従う。

### 分岐: match

`match cond { p1 => x1, ... }`

- cond とアームのパターン (`pi`) はすべて同じ型でなければいけない。
- アームの本体 (`xi`) はすべて同じ型でなければいけない。これが match の結果の型になる。

### ループ: while

`while cond { x }` は `loop { if cond { x } else { break } }` と同じ規則に従う。

### ループ: loop

`loop { ... }`

- loop の結果型は、すべての break の引数と同じ型でなければいけない。
    - はじめ、loop の結果型はフレッシュな型変数 T である。
    - このループに対応する break が1つ出現するたび、その引数の型を T に単一化する。

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
