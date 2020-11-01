# 型検査 (型推論)

WIP

構文から式やシンボルの型を計算し、検査する処理を型検査 (あるいは型推論) と呼んでいる。

型検査には検査と計算の2つの目的がある:

- 型つけ規則に違反する式を発見し、コンパイルエラーを報告する。
- 型の制約を計算し、型注釈を持たないシンボルの型を決定する。

型検査の前提となる言語の性質がある:

- エイリアスとローカル変数以外のシンボルの型はすべて型注釈が必須であり、型を推論する必要はない。
    (関数のパラメータや結果の型、構造体のフィールドの型など。クロージャを導入したらこの前提は崩れる。)

## 定義の型と式の型

fn 文などにより定義される、シンボル本来の型を定義の型と呼ぶ。(TODO: おそらく型スキームと呼んだほうがよい。)
例えば文 `fn memcmp[T](p: *T, q: *T, n: usize) -> i32;` によって定義される memcmp 関数の定義の型は `fn[T](p: *T, q: *T, n: usize) -> i32` という多相型になる。

定義の型は式の型とは異なることがある。
例えば式 `memcmp(s, "hi", 2)` における memcmp の式の型は `fn(*c8, *c8, usize) -> i32` になる。

## 期待される型

型検査は抽象構文木を評価と同じ順に辿り、訪問された式やパターンの型を貪欲に決めていく。

式を型検査するとき、その式に期待される型が指定されることがある。
「期待される型」はリテラルや多相なシンボルの型を決定するためのヒントとして利用される。
ただし「期待される型」は制約ではなく、期待される型とは異なる型がつくこともありうる。

例えば式 `x == y` を型検査するとき、左辺 x の型が i32 になったとする。
両辺は同じ型でなければいけないので、右辺 y の型検査に際し、期待される型として i32 が指定される。

記法:

- 式 x に期待される型 X が指定されたとき x の型が T になることを `x: T (expected X)` と書く。
- 式 x に期待される型 X が指定されたとき、x の型が S になって、それが型 T の部分型でなければいけない、ということを `x: S <: T (expected X)` と書く。

参考: [\[雑記\] 型の決定#型決定の「向き」](https://ufcpp.net/study/csharp/start/misctyperesolution/#source-target) (C# におけるターゲットからの型推論)

## 型の集合の略記

整数型の集合などを以下の通り略記する。

- iNN = `{i8, i16, i32, i64, i128, isize}`
- uNN = `{u8, u16, u32, u64, u128, usize}`
- fNN = `{f16, f32, f64, f128}`
- cNN = `{c8, c16, c32}`

## 補助的な関係や操作の定義

型は Sized という属性を持つ。この型の値を間接参照なしで持てることを表している。

暗黙的なアップキャストのための部分型関係と、明示的なキャストのための cast-to 関係がある。

部分型関係を考慮して分岐やループの結果型を決定するために、型の最小上界を求める join と、逆に最大下界を求める meet がある。

### Sized な型

`T: Sized` は型 T の値のサイズが分かることを表す。

```
(unit, bool, 数値型のサイズは分かる)
    T ∈ {unit, bool} ∪ iNN ∪ uNN ∪ fNN ∪ cNN
=>
    T: Sized

(ポインタのサイズは分かる)
    (*T): Sized,
    (*mut T): Sized

(関数のサイズは分かる)
    (fn(Ti, ...) -> U): Sized

(const enum のサイズ)
    E: const enum,
    T: E の表現の型,
    T: Sized
=>
    E: Sized

(struct enum)
    E: struct enum,
    ∀K: E のバリアント. K: Sized
=>
    E: Sized

(struct)
    K: struct,
    ∀f: K のフィールド. (
        T: フィールド f の型,
        T: Sized
    )
=>
    K: Sized
```

逆に unknown, never、型変数、未束縛なメタ型はサイズが分からない。

### join

join は複数の型の上界を求める、部分型関係における max のような操作。`∀i. Ti <: join(Ti, ...)` が成り立つ。

n 項の join は二項演算の join の畳み込みとして定める。単位元は never。

```
    join() = never
    join(T) = T
    join(T1, T2, Ui, ...) = join(join(T1, T2), Ui, ...)
```

```
    join(T, T) = T

(never は無視する)
    join(T, never) = T,
    join(never, T) = T

(読み取り専用のポインタ型の上界)
    join(*T, *mut U)
    = join(*mut T, *U)
    = *join(T, U)

(関数)
    join(fn(Ti, ...) -> U, fn(Xi, ...) -> Y)
    = fn(meet(Ti, Xi), ... ) -> join(Ti, Y)

(未束縛のメタ型)
    join(M, _): !
    join(_, M): !

(その他)
    join(_, _) = unknown
```

FIXME: struct, enum

### meet

meet は join の逆で、複数の型の下界を求める、部分型関係における min のような操作。`∀i. meet(Ti, ...) <: Ti` が成り立つ。

n 項の meet は二項演算の meet の畳み込みとして定める。単位元は unknown。

```
    meet() = unknown
    meet(T) = T
    meet(T1, T2, Ui, ...) = meet(meet(T1, T2), Ui, ...)
```

```
    meet(T, T) = T

(unknown は無視する)
    meet(T, unknown) = T,
    meet(unknown, T) = T

(読み取り専用のポインタ型の下界)
    meet(*T, *U)
    = *meet(T, U)

(可変なポインタ型の下界)
    meet(*mut T, *mut U)
    = meet(*mut T, *U)
    = meet(*T, *mut U)
    = *mut meet(T, U)

(関数)
    meet(fn(Ti, ...) -> U, fn(Xi, ...) -> Y)
    = fn(join(Ti, Xi), ... ) -> meet(Ti, Y)

(未束縛のメタ型)
    meet(M, _): !
    meet(_, M): !

(その他)
    meet(_, _) = never
```

FIXME: struct, enum

### 部分型関係

キャスト式を減らすため、値の実行時の表現に互換性がある場合にかぎって、暗黙のアップキャストを認める。

値を引数に渡したりフィールドに割り当てたり代入したりするとき、値の型が対象 (引数やフィールド) の型の部分型だったら、型が厳密に一致していなくても型検査が通る。
(そうなるように式の型つけ規則を書いている。)

型 T が型 U の部分型であることを `T <: U` と書き、以下の通り定める:

```
(反射性)
    T <: T

(unknown の部分型関係)
    T <: unknown

(never の部分型関係)
    never <: T

(読み取り専用のポインタの共変性)
    T <: U
=>
    *T <: *U

(可変なポインタから読み取り専用のポインタへの降格)
    T <: U
=>
    *mut T <: *U

(関数の部分型関係)
    Ti <: Xi,
    Y <: U
=>
    (Ti, ...) -> U <: (Xi, ...) -> Y
```

備考:

- 部分型関係は値の表現を保つので、いわゆる型強制は生じない。
    (実行時の処理が必要な場合は as を使って明示的にキャストする方針。)
- 推移的に適用することはできない。
    (部分型関係を検査するときに結論から前提を逆算できるようにするため。)
- 部分型関係が要求される際にどちらかまたは両方が未束縛なメタ型なら、両者を単一化する。
- `extern fn free(ptr: *mut unknown);` のために `*mut T <: *mut unknown` という関係を定めた方がよいかもしれないが、採用していない。
    - `*mut T` の上界が2種類あるとややこしい。
    - そのユースケースは `extern fn free[T](ptr: *mut T);` で代用できる。

FIXME: ジェネリックな struct/enum の型パラメータに関する共変性・反変性を追加したい

### 制限

部分型関係 `T <: U` を検査するとき、型 T を型 U に制限する。型 T が型 U の部分型であることを検査するだけでなく、必要に応じてメタ型を束縛する。
(HM 型推論における単一化に相当する。)

### キャストの条件

型の二項関係 `T cast-to U` を次のように定める:

```
(部分型から上位型にキャスト可能)
    T <: U
=>
    T cast-to U

(任意の型は unit にキャスト可能)
    T cast-to unit

(bool と整数型は相互にキャスト可能)
    T, U ∈ {bool} ∪ iNN ∪ uNN
=>
    T cast-to U

(数値型は相互にキャスト可能)
    T, U ∈ iNN ∪ uNN ∪ fNN ∪ cNN
=>
    T cast-to U

(ポインタサイズの型は相互にキャスト可能)
    T, U ∈ {*X, *mut X, isize, usize}
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
    T ∈ iNN ∪ uNN ∪ fNN ∪ cNN,
    n: 型接尾辞 T を持つ数値リテラル
=>
    n: T
```

型接尾辞がないときは期待される型がつく。ただし小数リテラルは浮動小数点数型になる。

```
    n: 型接尾辞を持たない、小数部か指数部を持つ数値リテラル,
    T ∈ fNN
=>
    n: T (expected T)
```

それ以外はいずれかの数値型がつく。

```
    n: 型接尾辞や小数部や指数部を持たない数値リテラル,
    T ∈ iNN ∪ uNN ∪ fNN ∪ cNN
=>
    n: T (expected T)
```

ただし、リテラルに書かれた数を正確に表現できない整数型および文字型はつかない。(浮動小数点数型は正確でなくてもよい。)

リテラルに期待される型と上記の制約から型が一意に決定できなければ、型エラーになる。

### 文字リテラル

数値リテラルと同様。

```
    T ∈ cNN,
    c: 型接尾辞 T を持つ文字リテラル
=>
    c: T
```

```
    c: 型接尾辞を持たない文字リテラル,
    T ∈ cNN
=>
    c: T (expected T)
```

期待される型から決定できないとき、1バイトで表現できる文字は c8 に推論する。

```
    c: 型接尾辞を持たない、1バイトで表現できる文字リテラル,
    X ∉ cNN
=>
    c: c8 (expected X)
```

1バイトで表現できない文字なら c32 に推論する。

```
    c: 型接尾辞を持たない、1バイトで表現できない文字リテラル,
    X ∉ cNN
=>
    c: c32 (expected X)
```

### 文字列リテラル

文字リテラルと同様。

```
    T ∈ cNN,
    s: 型接尾辞 T を持つ文字列リテラル
=>
    s: *T
```

```
    s: 型接尾辞を持たない文字列リテラル,
    T ∈ cNN
=>
    s: *T (expected *T)
```

期待される型から決定できないときは `*c8` に推論する。

```
    s: 型接尾辞を持たない文字列リテラル,
    X ∉ {*c8, *c16, *c32}
=>
    s: *c8 (expected X)
```

FIXME: 文字列リテラルを `Str` などの構造体に直に推論する規則を追加したい

## パターンの型つけ規則

いまのところ自明なので略。

- リテラルパターン: リテラル式の型つけと同様
- 変数パターン・ワイルドカードパターン: 期待される型から決める
- レコードパターン: レコードの型になる

## 式の型つけ規則

### 識別子・パス・型適用

シンボルに解決された識別子やパスの式には、そのシンボルの定義の型がつく。

定義の型が多相のとき、型変数にそれぞれ型引数リストで指定された型を割り当てて、定義の型に型変数を代入する。
型引数リストがなければ、代わりに、それぞれの型引数としてフレッシュなメタ型が指定されたとみなす。
その後、シンボルの式の型を期待される型に制限する。

変数の参照は可変な式である。

```
    n: ローカル変数か static 変数
=>
    n: 可変な式
```

### レコード式

(関数呼び出しと同様。)

それぞれのフィールドに割り当てられる式には、フィールドの型が期待される。
その式の型はフィールドの型の部分型でなければいけない。

```
    K: レコード構造体型またはレコードバリアント,
    ∀i. (
        fi: K のフィールド,
        Ti: フィールド fi の型,
        xi: _ <: Ti (expected Ti),
    )
=>
    K { fi: xi, ... }: T (expected X)

ただし
    K が構造体なら T = K,
    K がバリアントなら、T はその enum の型
```

K の定義の型が多相のときは「識別子・パス・型適用」の節と同じ方法で式の型を決定する。
(K に期待される型として X を指定する。)

### フィールド式

```
    K: レコード型,
    f: K のフィールド,
    T: フィールド f の型,
    T: Sized,
    x: K,
=>
    x.f: T
```

FIXME: ポインタを自動で脱参照する？

x の可変性が伝播する。

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
    f(xi, ...): U
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
    (x as U): U
```

### 単項演算: `!x`

`!x` は、x が bool または整数型でなければいけない。結果は x と同じ型になる。

```
    x: T (expected {X} ∩ ({bool} ∪ iNN ∪ uNN)),
    T ∈ {bool} ∪ iNN ∪ uNN,
=>
    !x: T (expected X)
```

### 単項演算: `-x`

```
    x: T (expected {X} ∩ (iNN ∪ fNN)),
    T ∈ iNN ∪ fNN
=>
    -x: T (expected X)
```

#### 単項演算: `*x`

`*x` は x がポインタ型である必要がある。

```
    x: T (expected *X),
    ∃U: Sized. T ∈ {*U, *mut U}
=>
    *x: U (expected X)
```

```
    x: *mut T
=>
    *x: 可変な式
```

x に期待される型は `*mut X` ではなく `*X` を指定する。期待される型はある型の上位型かどうかが問題になるので、`join(*T, *mut T) = *T` を指定しておけば十分なため。

#### 単項演算: `&x`

```
    x: T (expected X)
=>
    &x: *T (expected *X)
```

`&mut x` は x の可変性を要求する:

```
    x: T (expected X),
    x: 可変な式
=>
    &mut x: *mut T (expected *mut X)
```

### 二項演算: 和

`x + y` は、x が数値型のときは y も同じ型でなければいけない。結果は x と同じ型になる。

```
    x: T (expected X),
    y: T (expected T),
    T ∈ iNN ∪ uNN ∪ fNN ∪ cNN
=>
    (x + y): T (expected X)
```

x がポインタ型のときは、y はポインタサイズの整数型でなければいけない。結果は x と同じ型になる。

```
    x: T (expected X),
    y: _ ∈ {isize, usize} (expected isize),
    ∃U: Sized. T ∈ {*U, *mut U},
=>
    (x + y): T (expected X)
```

### 二項演算: 差

`x - y` は `x + y` と同様の2つの規則を持つ。さらに、x がポインタ型のとき y も同じポインタ型になることがある。

```
    x: T (expected X),
    y: T (expected T),
    T ∈ iNN ∪ uNN ∪ fNN ∪ cNN
=>
    (x + y): T (expected X)
```

```
    x: T (expected X),
    y: _ ∈ {isize, usize} (expected isize),
    ∃U: Sized. T ∈ {*U, *mut U}
=>
    (x + y): T (expected X)
```

```
    x: T (expected X),
    y: T (expected isize),
    ∃U: Sized. T ∈ {*U, *mut U}
=>
    (x - y): isize (expected X)
```

(この規則では期待される型 X は役に立たないが、x の型検査を行う段階では数値に関する規則と弁別できていないので、期待される型を分けることはできない。)

### 二項演算: 積

積などは整数型と小数型に入る。

```
    x: T (expected X),
    y: T (expected T),
    T ∈ iNN ∪ uNN ∪ fNN
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
    T ∈ iNN ∪ uNN ∪ cNN
=>
    (x & y): T (expected X),
    (x | y): T (expected X),
    (x ^ y): T (expected X)
```

シフトは左辺が整数型と文字型で、右辺は整数型でなければいけない。左右の型は異なってもよい。右辺は既定で u32 に推論される。

```
    x: T (expected X),
    y: _ ∈ iNN ∪ uNN ∪ cNN (expected u32),
    T ∈ iNN ∪ uNN
=>
    (x << y): T (expected X),
    (x >> y): T (expected X)
```

### 二項演算: 比較

```
    x: T,
    y: T (expected T),
    T is-equatable
=>
    (x == y): bool,
    (x != y): bool
```

```
    x: T,
    y: T (expected T),
    T is-comparable
=>
    (x < y): i32,
    (x <= y): i32,
    (x > y): i32,
    (x >= y): i32
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

(*dangling_ptr() && y)

- 備考: 期待される型として bool を指定する意味があるケースは少ない。
    - わざとらしい例なら作れる: `fn zero_ptr[T]() -> *T { static ZERO: usize = 0; (&ZERO) as *T }`, `(*zero_ptr() && false)`

### 代入式

```
    x: T,
    x: 可変な式,
    y: _ <: T (expected T)
=>
    (x = y): unit
```

`x += y` などの複合代入演算は、結果型が unit になることと、左辺に期待する型を指定できないことを除いて、対応する二項演算と同様の規則を持つ。

### ブロック式

空のブロック式は unit に等しい。

```
    {}: unit
```

空でないブロックは、最後が式文でなければ unit に評価される。途中の文は unit の部分型でなければいけない。
(unit の部分型とは要するに never で、`(return;): never <: unit` を許可している。)

```
    dn: (ブロックの最後の文) 式文でない,
    ∀i. di: _ <: unit (expected unit)
=>
    { di; ... }: unit
```

最後が式文なら、ブロックに期待される型はその式に引き継がれる。ブロックの結果は、その式の結果に等しい。

```
    ∀i. di: _ <: unit,
    x: T (expected X)
=>
    { di; ...; x; }: T (expected X)
```

### ジャンプ式

ジャンプの結果型は never になる。

```
    (break x): never,
    continue: never,
    (return x): never
```

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

最終的に、すべてのアームの本体の型の最小上界 (join) が結果になる。
(これにより、アームの本体の型が match の結果の型の部分型になることが保証される。)

```
    cond: T,
    ∀i. (
        pi: _ <: T (expected T),
        xi: Ui (expected join(X, U1, ..., U(i-1))),
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
    while cond { x }: unit
```

### ループ: loop

loop の結果型は break の引数の型の join になる。

loop に期待される型は、そのループをターゲットとする break の引数にも期待される。
加えて、それより前に出現した break の引数の型の join が期待される。
最終的に loop の型は、break の引数の型の join になる。
(match と同様。)

```
    x: _ <: unit (expected unit),
    ∀(break xi). xi: Ti (expected join(X, T1, ..., T(i-1))),
    T = join(Ti, ...)
=>
    loop { x }: T (expected X)
```

## 文の型つけ規則

式文以外は unit 型がつく。

### 式文

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
    (let p: T = x;): unit
```

型注釈がないときは初期化式の型がパターンの型になる。

```
    x: T,
    p: _ <: T (expected T),
=>
    (let p = x;): unit
```

### const, static

let と同様。(型注釈は必須。)

### fn 文

fn 文の本体には、結果型として指定された型が期待される。
本体の型は結果型の部分型でなければいけない。

この関数をターゲットとする return の引数にも、結果型として指定された型が期待される。

```
    x: _ <: U (expected U),
    ∀(return xi). xi: _ <: U (expected U),
=>
    (fn f(pi: Ti, ...) -> U { x }): unit
```

### extern fn

fn と同様。

### enum, struct, use

特になし。
