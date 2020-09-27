# 型検査 (型推論)

WIP

構文から式やシンボルの型を計算し、検査する処理を型検査 (あるいは型推論) と呼んでいる。

型検査には検査と計算の2つの目的がある:

- 型つけ規則に違反する式を発見し、コンパイルエラーを報告する。
- 型の制約を計算し、型注釈を持たないシンボルの型を決定する。

型検査の前提となる言語の性質がある:

- エイリアスとローカル変数以外のシンボルの型はすべて型注釈が必須であり、型を推論する必要はない。
    (関数のパラメータや結果の型、構造体のフィールドの型など。クロージャを導入したらこの前提は崩れる。)

TODO: 概要を詳しく書く

- 型検査の流れを書く
    (型検査は基本的に、式を評価順にみていき、それぞれの部分式の型を貪欲に決めていく。ただし部分的に、評価の「結果」側から式の型を決定する部分もある。)
- 式やパターンに「期待される型」について書く
    - 式 e に期待される型が X のとき、e の型が T になることを `e: T (expected X)` と書く
    - 期待される型はヒントであって制約ではない。
        実際の型が「期待される型」に一致するか、あるいは部分型になるか、という検査は意図していない。
    - 式の型が持つべき条件は、期待される型とは別に、実際の型に関する条件を記述する。これが破られたら型エラーを起こす。
    - 式 e に期待される型が X のとき、e の型が S になって、それが型 T の部分型でなければいけない、ということを `e: S <: T (expected X)` と書く
- 式の型は、その式に要求される制約が一意に決まるときのみ型が決定する。
- 型検査の途中では bool | iNN などの和型が出現する。シンボルの宣言の型は和型にはならない。

### 宣言の型と式の型

fn 宣言などにより定義される、シンボル本来の型を宣言の型と呼ぶ。
例えば `extern fn abort() -> never` の宣言の型は `fn() -> never` という矢印型になる。

宣言の型は式の型とは異なる可能性がある。
例えば never の部分型つけに関する規則のため、式 `abort() as i32` における abort の「式の型」は `fn() -> i32` になる。

### 具体的な型

以下の型は具体的な型と呼ぶ。

- never, unit, bool
- `*unknown`, `*mut unknown`
- 数値型 (iNN, uNN, fNN, cNN)
- enum 型
- struct 型
- 具体的な型へのポインタ型 (`*i32`, `*mut i32` など)
- 具体的な型からなる矢印型 (`fn() -> i32` など)
- 具体的な型に解決されたメタ型

unknown と、具体的な型に解決されていないメタ型は具体的な型ではない。

## 補助的な関係や操作の定義

メタ型を扱うため、単一化がある。

部分型関係の定義のため、補助的に属性 `T is-empty` と型の二項演算 `T \ U` がある。

### 単一化

TODO

### 空の型

```
(never は空)
    never is-empty

(空の型の和も空)
    ∀i. Ti is-empty
=>
    (Ti | ...) is-empty
```

### 型の引き算

```
    U is-empty
=>
    T \ U = T

    T <: U
=>
    T \ U = never

    *T \ *U = *(T \ U)

    *mut T \ *mut U = *mut (T \ U)

    (Ti, ...) -> U \ (Xi, ...) -> Y
    = TODO

    T \ (Ui | ...) = T \ Ui \ ...

    (Ti | ...) \ U = ((Ti \ U) | ...)
```

TODO

### join

TODO

join は複数の型の上界を求める、部分型関係における max のような操作。`∀i. Ti <: join(Ti, ...)` が成り立つ。

n 項の join は二項演算の join の畳み込みとして定める。単位元は never。

```
    join() = never
    join(T) = T
    join(T1, T2, Ui, ...) = join(join(T1, T2), Ui, ...)
```

```
    join(T, T) = T

(空の型は無視する)
    U is-empty
=>
    join(T, U) = T,
    join(U, T) = T

(unknown による吸収)
    join(T, unknown) = unknown,
    join(unknown, T) = unknown,
    join(*mut T, *mut unknown) = *mut unknown,
    join(*mut unknown, *mut T) = *mut unknown

(読み取り専用のポインタ型の上界)
    join(*T, *mut U)
    = *join(T, U)

(関数)
    TODO

(和の上界は上界の和)
    join((Ti | ...), U) = (join(Ti, U) | ...),
    join(T, (Ui | ...)) = (join(T, Ui) | ...)
```

### meet

TODO

### 部分型関係

型 T, U の部分型関係 `T <: U` が以下の条件で成り立つ。

```
(反射性)
    T <: T

(unknown の部分型関係)
    T <: unknown,
    *mut T <: *mut unknown

(空の型の部分型関係)
    T is-empty
=>
    T <: U

(読み取り専用のポインタの共変性)
    T <: U
=>
    *T <: *U

(読み取り専用のポインタへの降格)
    T <: U
=>
    *mut T <: *U

(関数の部分型関係)
    Ti <: Xi,
    Y <: U
=>
    (Ti, ...) -> U <: (Xi, ...) -> Y

(和型の部分型関係)
    (U \ Ti \ ...) is-empty
=>
    U <: (Ti | ...)
```

代入などの際に、部分型関係を使って暗黙の「アップキャスト」が起こるようになっている。
部分型関係は値の表現を保つので、いわゆる型強制は生じない。
(実行時の処理が必要な場合は as を使って明示的にキャストする方針。)

推移的に適用することはできない。
(部分型関係を検査するときに結論から前提を逆算できるようにするため。)

部分型関係を検査する際にどちらかまたは両方が未束縛なメタ型なら、バインドする。

FIXME: ジェネリック型の型パラメータに関する共変性・反変性を追加したい

### キャストの条件

型の二項関係 `T cast-to U` を次のように定める:

```
(部分型つけ可能ならキャスト可能)
    T <: U
=>
    T cast-to U

(任意の型は unit にキャスト可能)
    T cast-to unit

(数値型は相互にキャスト可能)
    T, U <: (bool | iNN | uNN | fNN | cNN)
=>
    T cast-to U

(ポインタサイズの型は相互にキャスト可能)
    T, U <: (*unknown | *mut unknown | isize | usize)
=>
    T cast-to U

(const enum は表現の型と相互にキャスト可能)
    E: const enum 型,
    T: E の表現の型
=>
    E cast-to T,
    T cast-to E
```

## リテラルの型つけ規則

リテラルの型つけ規則は式とパターンのどちらでも同様なので、まとめて書く。

### unit, true, false リテラル

```
    unit: unit,
    true: bool,
    false: bool
```

### 数値リテラル

型接尾辞を持つ数値リテラルはその型を持つ。(例えば 42_i32 なら型は i32 になる。)

```
    T <: (iNN | uNN | fNN | cNN),
    n: 型接尾辞 T を持つ数値リテラル
=>
    n: T (expected _)
```

型接尾辞がないときは期待される型がつく。ただし小数リテラルは浮動小数点数型になる。

```
    n: 型接尾辞を持たない、小数部か指数部を持つ数値リテラル,
    T <: fNN
=>
    n: T (expected T)
```

それ以外はいずれかの数値型がつく。

```
    n: 型接尾辞や小数部や指数部を持たない数値リテラル,
    T <: (iNN | uNN | fNN | cNN)
=>
    n: T (expected T)
```

ただし、リテラルに書かれた数を正確に表現できない整数型および文字型は、制約の上界から除外される。(浮動小数点数型は正確でなくてもよい。)

リテラルに期待される型と上記の制約から型が一意に決定できなければ、型エラーになる。

### 文字リテラル

数値リテラルと同様。

```
    T <: cNN,
    c: 型接尾辞 T を持つ文字リテラル
=>
    c: T (expected _)
```

```
    c: 型接尾辞を持たない文字リテラル,
    T <: (c8 | c16 | c32)
=>
    c: T (expected T)
```

期待される型から決定できないときは、1バイトで表現できる文字は c8 に推論する。

```
    c: 型接尾辞を持たない、1バイトで表現できる文字リテラル,
    ￢(X <: (c8 | c16 | c32))
=>
    c: c8 (expected X)
```

1バイトで表現できない文字は c32 に推論する。

```
    c: 型接尾辞を持たない、1バイトで表現できない文字リテラル,
    ￢(X <: (c8 | c16 | c32))
=>
    c: c32 (expected X)
```

### 文字列リテラル

文字リテラルと同様。

```
    T <: cNN,
    s: 型接尾辞 T を持つ文字列リテラル
=>
    s: *T (expected _)
```

```
    s: 型接尾辞を持たない文字列リテラル,
    T <: cNN
=>
    s: *T (expected *T)
```

期待される型から決定できないときは `*c8` に推論する。

```
    s: 型接尾辞を持たない文字列リテラル,
    ￢(X <: (*c8 | *c16 | *c32))
=>
    s: *c8 (expected X)
```

FIXME: 文字列リテラルを `Str` などの構造体に直に推論する規則を追加したい

## パターンの型つけ規則

いまのところ自明なので略。

## 式の型つけ規則

### 識別子・パス・型適用

シンボルに解決された識別子やパスの式には、そのシンボルの宣言の型がつく。

宣言の型が多相のとき:

- 型引数リストを持つなら、型変数をそれぞれ型引数に指定された型で置き換える。
- 型引数リストがなければ、インスタンス化を行う。
    (型変数をそれぞれフレッシュなメタ型で置き換える。)
    (FIXME: 期待される型と unify する？)

変数の参照は可変な式である。

```
    n: ローカル変数か static 変数
=>
    n: 可変な式
```

### レコード式

それぞれのフィールドに割り当てられる式には、フィールドの型が期待される。
その式の型はフィールドの型の部分型でなければいけない。

```
    K: レコード型,
    ∀i. (
        fi: K のフィールド,
        Ti: フィールド fi の型,
        xi: _ <: Ti (expected Ti),
    )
=>
    K { fi: xi, ... }: T (expected _)

ただし
    K が構造体なら T = K,
    K が enum E のバリアントなら T = E
```

### フィールド式

```
    K: レコード型,
    f: K のフィールド,
    T: フィールド f の型,
    x: K,
=>
    x.f: T (expected _)
```

FIXME: ポインタを自動で脱参照する？

x の可変性が遺伝する。

```
    x: 可変な式
=>
    x.f: 可変な式
```

### 関数呼び出し

それぞれの引数に割り当てられる式には、パラメータの型が期待される。
その式の型はパラメータの型の部分型でなければいけない。

```
    f: (T1, T2, ...) -> U,
    ∀i. xi: _ <: Ti (expected Ti)
=>
    f(xi, ...): U (expected _)
```

### 添字式

`x[i]` は `*(x + i)` と同じ規則に従う。

```
    *(x + i): T (expected X)
=>
    x[i]: T (expected X)
```

```
    *(x + i): 可変な式
=>
    x[i]: 可変な式
```

### キャスト式

```
    x: T (expected U),
    T cast-to U
=>
    (x as U): U (expected _)
```

### 単項演算

`!x` は、x が bool または整数型でなければいけない。結果は x と同じ型になる。

```
    x: T (expected (X & (bool | iNN | uNN))),
    T <: (bool | iNN | uNN),
=>
    !x: T (expected X)
```

`-x` も同様。

```
    x: T (expected (X & (iNN | fNN))),
    T <: (iNN | fNN)
=>
    -x: T (expected X)
```

`*x` は x がポインタ型である必要がある。

```
    x: T (expected (*X & (*unknown | *mut unknown))),
    (T = *U または T = *mut U)
=>
    *x: U (expected X)
```

```
    x: *mut T (expected 同上)
=>
    *x: 可変な式
```

`&x`:

```
    x: T (expected X)
=>
    &x: *T (expected *X)
```

`&mut x` は x の可変性を検査する:

```
    x: T (expected X),
    x: 可変な式
=>
    &mut x: *mut T (expected X)
```

### 二項演算: 和

`x + y` は、x が数値型のときは y も同じ型でなければいけない。結果は x と同じ型になる。

```
    x: T (expected X),
    y: T (expected T),
    T <: (iNN | uNN | fNN | cNN)
=>
    (x + y): T (expected X)
```

x がポインタ型のときは、y はポインタサイズの整数型でなければいけない。結果は x と同じ型になる。

```
    x: T (expected X),
    y: _ <: (isize | usize) (expected isize),
    T <: (*unknown | *mut unknown)
=>
    (x + y): T (expected X)
```

### 二項演算: 差

`x - y` は `x + y` と同様の2つの規則を持つ。さらに、x がポインタ型のとき y も同じポインタ型になることがある。

```
    x: T (expected X),
    y: T (expected T),
    T <: (iNN | uNN | fNN | cNN)
=>
    (x + y): T (expected X)
```

```
    x: T (expected X),
    y: _ <: (isize | usize) (expected (isize | T)),
    T <: (*unknown | *mut unknown)
=>
    (x + y): T (expected X)
```

(次の規則のため、y の expected に T をいれておく必要がある。
これにより数値リテラルの推論が阻害されることはないはず。)

```
    x: T (expected X),
    y: T (expected (isize | T)),
    T <: (*unknown | *mut unknown)
=>
    (x - y): isize (expected X)
```

(この規則では期待される型 X は役に立たないが、x の型検査を行う段階では数値に関する規則と弁別できていないので、x につける expected はそろえる必要がある。)

### 二項演算: 積

積などは整数型と小数型に入る。

```
    x: T (expected X),
    y: T (expected T),
    T <: (iNN | uNN | fNN)
=>
    (x * y): T (expected X),
    (x / y): T (expected X),
    (x % y): T (expected X)
```

### 二項演算: ビット演算

シフト以外のビット演算は整数型と文字型に入る。

```
    x: T (expected X),
    y: T (expected T),
    T <: (iNN | uNN | cNN)
=>
    (x & y): T (expected X),
    (x | y): T (expected X),
    (x ^ y): T (expected X)
```

シフトは左辺が整数型と文字型で、右辺は整数型でなければいけない。左右の型は異なってもよい。右辺は既定で u32 に推論される。

```
    x: T (expected X),
    y: _ <: (iNN | uNN) (expected u32),
    T <: (iNN | uNN | cNN)
=>
    (x << y): T (expected X),
    (x >> y): T (expected X)
```

### 二項演算: 比較

```
    x: T (expected _),
    y: T (expected T),
    T is-equatable
=>
    (x == y): bool (expected _),
    (x != y): bool (expected _)
```

```
    x: T (expected _),
    y: T (expected T),
    T is-comparable
=>
    (x < y): i32 (expected _),
    (x <= y): i32 (expected _),
    (x > y): i32 (expected _),
    (x >= y): i32 (expected _)
```

- FIXME: is-equatable, is-comparable の定義を書く。
- FIXME: レコード型の比較に関する型つけ規則を定める。

同値比較が可能な型の条件:

- bool, 数値型, const enum
- (TODO: 同値比較が可能な型からなるレコード？)

順序比較が可能な型の条件:

- bool, 数値型, const enum
- (TODO: 順序比較が可能な型からなるレコード？)

### 二項演算: 論理

```
    x: bool (expected bool),
    y: bool (expected bool)
=>
    (x && y): bool,
    (x || y): bool
```

### 代入

```
    x: T (expected _),
    x: 可変な式,
    y: _ <: T (expected T)
=>
    (x = y): unit (expected _)
```

`x += y` などは二項演算と同様。

### ブロック

空のブロックは unit に等しい。

```
    {}: unit
```

空でないブロックは、最後が式宣言でなければ unit に評価される。途中の宣言は unit の部分型でなければいけない。
(unit の部分型とは要するに never で、`(return;): never <: unit` を許可している。)

```
    dn: (ブロックの最後の宣言) 式宣言でない,
    ∀i. di: _ <: unit
=>
    { di; ... }: unit (expected _)
```

最後が式宣言なら、ブロックに期待される型はその式に引き継がれる。ブロックの結果は、その式の結果に等しい。

```
    ∀i. di: _ <: unit,
    x: T (expected X)
=>
    { di; ...; x; }: T (expected X)
```

### ジャンプ

break, continue, return の結果は never 型になる。

break, return の引数式に関する型つけ規則は loop, fn を参照。

### 分岐: if

else 節がないとき、if 式の結果は unit になる。ここで x は then 節のブロック式を表す。

```
    cond: bool (expected bool),
    x: _ <: unit (expected unit)
=>
    if cond { x }: unit (expected X)
```

else 節があるときは match と同様。

```
    match cond {
        true => x,
        false => y,
    }: T (expected X)
=>
    if cond { x } else { y }: T (expected X)
```

### 分岐: match

`match cond { ... }` は、cond の型に特に条件はないが、パターンにはその型が期待されて、パターンの型はそれの部分型でなければいけない。

match 式に期待される型はアームの本体にも期待される。
加えて、それより前に書かれたアームの本体の型も期待される。

最終的に、すべてのアームの本体の型の join が結果になる。
(これにより、アームの本体の型が match の結果の型の部分型になることが保証される。)

```
    cond: T (expected _),
    ∀i. (
        pi: _ <: T (expected T),
        xi: Ui (expected (X & join(U1, ..., U(i-1)))),
    ),
    U = join(Ui, ...)
=>
    match cond { pi => xi, ... }: U (expected X)
```

### ループ: while

```
    cond: bool (expected bool),
    x: _ <: unit (expected unit)
=>
    while cond { x }: unit (expected _)
```

### ループ: loop

loop の結果型は break の引数の型の join になる。

loop に期待される型は、そのループをターゲットとする break の引数にも期待される。
加えて、それより前に出現した break の引数の型の join が期待される。
最終的に loop の型は、break の引数の型の join になる。
(match と同様。)

```
    x: _ <: unit (expected unit),
    ∀(break xi). xi: Ti (expected (X & join(T1, ..., T(i-1)))),
    T = join(Ti, ...)
=>
    loop { x }: T (expected X)
```

## 宣言の型つけ規則

### 式宣言

```
    e: T (expected X)
=>
    (e;): T (expected X)
```

### let

型注釈があるとき、初期化式にはその型が期待される。

```
    x: _ <: T (expected T),
    p: _ <: T (expected T)
=>
    (let p: T = x;): unit (expected _)
```

型注釈がないときは初期化式の型がパターンの型になる。

```
    x: T (expected _),
    p: _ <: T,
=>
    (let p = x;): unit (expected _)
```

### const, static

let と同様。(型注釈は必須。)

### fn 宣言

fn 宣言の本体には、結果型の型が期待される。
その型は結果型の部分型でなければいけない。

この関数をターゲットとする return の引数には、結果型の型注釈に書かれた型が期待される。

```
    x: _ <: U (expected U),
    ∀(return xi). xi: _ <: U (expected U),
=>
    (fn f(pi: Ti, ...) -> U { x }): unit (expected _)
```

### extern fn

fn と同様。

### struct, enum, use

特になし。
