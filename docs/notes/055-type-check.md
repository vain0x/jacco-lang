# 型の関係と規則

WIP: 構文木上での型検査に切り替えるかもしれない

型検査は構文木ではなく CPS 中間表現の上で行う。

## 部分型関係

S, T を型とするとき、S が T の部分型であることを `S <: T` と書く。

```rust
    // 反射性
    T <: T

    // ! が bottom 型であること
    ! <: T

    // 読み取り専用のポインタ型の共変性
    S <: T => *S <: *T

    // 可変なポインタから読み取り専用のポインタへの昇格
    S <: T => *mut S <: *T

    // *unknown への昇格
    *T <: *unknown
    *mut T <: *unknown
```

部分型関係は型つけ規則で利用される。パラメータやフィールドへの束縛時に、部分型の値は暗黙的に「アップキャスト」される。実行時の振る舞いはいわゆる部分集合意味論であり、型強制は生成されない。(キャストに際して値への変換操作が必要になるものは `as` 演算子を要求している。)

## 項の型つけ規則

### `()`, true, false の型つけ規則

```
    Γ |-
        () : (),
        true : bool,
        false : bool.
```

### 数値リテラルの型つけ規則

数値リテラルの型は以下のように決める:

- 型サフィックスを持つなら、そのサフィックスが指定する型である。例: `42_i32 : i32`
- 値が i64, u64 のどちらでも厳密に表現できないなら、fNN である。例: `3.14 : fNN`
- 値が u64 で表現できないなら、iNN である。例: `-1 : iNN`
- いずれでもなければ uNN である。例: `42 : uNN`

### 文字リテラルの型つけ規則

文字リテラルの型は以下のように決める:

- 型サフィックスを持つなら、そのサフィックスが指定する型である。例: `'a'_c8 : c8`
- そうでなければ cNN である。

### 文字列リテラルの型つけ規則

TBD

## ノードの型つけ規則

環境 Γ においてノードが型検査を通ることを次のように表す。

```
    Γ |- k : well-typed
```

CPS ノードは次のように表す。ただし型パラメータのリスト `[tys...]` や継続のリスト `[conts...]` は空のとき省略する。

```
    [kind[tys...] [args...] [results...] [conts...]]
```

型を表すメタ変数は、特に明記しなければ Sized (静的にサイズが確定する型) に制限される。

### let

Γ を環境とし、t を項とし、u をシンボルとし、k をノードとするとき、次が成り立つ。

```
    Γ        |- t : T,
    Γ, u : T |- k : well-typed
=>
    Γ |- [let [t] [u] [k]] : well-typed
```

これは「環境 Γ において項 t に型 T がつき、さらに Γ を `u : T` で拡張した環境においてノード k が型検査を通るなら、let ノード `[let [t] [u] [k]]` は型検査を通る」を意味する。

### ジャンプ

(引数が2個で継続が1個のケースだけ書く。)

f をラベルとし、p1, p2 を f のパラメータとし、t1, t2 を項とし、T1, T2 を型とし、k をノードとするとき、次が成り立つ。

```
    // 引数と本体に型がつくこと
    Γ |-
        t1 : T1,
        t2 : T2,
        k : well-typed,
    // パラメータの型が引数の型の上位型になること
    T1 <: P1, T2 <: P2,
    p1 : P1, p2 : P2 |-
        f : well-typed
=>
    Γ |- [jump [f t1 t2] [] [k]] : well-typed
```

ラベルのパラメータは分岐やループの結果を受け取る一時変数なので、構文的な型注釈はついていないし、型検査も終わっていない。そのため、ジャンプの引数の型から逆算する。

ラベルへの引数を伴うジャンプはすべてジャンプ先のラベルより前のラベルで発生する。ラベル自身の本体や後ろのラベルから来るジャンプは `continue` に限られて、これは引数を伴わない。そのため、ラベルの型検査を開始する時点で、ラベルのパラメータの型は確定できる。ジャンプで与えられた引数の型を覚えておき、それらの結びを取ればよい。

### 関数呼び出し

(引数が2個のケースだけ書く。)

関数は構文的に型注釈が必須なので、関数のパラメータや結果の型は確定している。引数は部分型関係を用いて暗黙的にアップキャストする。

T1, T2, P1, P2, U を型とし、f を関数とし、(p1 : P1, p2: P2) を f のパラメータリストとし、u をシンボルとし、k をノードとするとき:

```
    Γ |-
        f : fn(P1, P2) -> U,
        t1 : T1 <: P1,
        t2 : T2 <: P2,
    Γ, u : U |- k : well-typed
=>
    Γ |- [call-direct [f t1 t2] [u] [k]] : well-typed
```

### 参照式

```
    Γ |- t : T,
    Γ, u : *T |- k : well-typed
=>
    Γ |- [ref [t] [u] [k]] : well-typed
```

`mut` の方も同様。

### 脱参照式

```
    Γ |- t : *T,  // or *mut T
    Γ, u : T |- k : well-typed
=>
    Γ |- [deref [t] [u] [k]] : well-typed
```

### レコード構築

(フィールドが2個のケースだけ書く。)

F1, F2, T1, T2 を型とし、U をレコードの型とし、f1, f2 をレコードのフィールドとするとき、次が成り立つ。

```
    |-
        f1 : F1,    // フィールドの型
        f2 : F2,
    // 引数がフィールドの型の部分型であること
    Γ |-
        t1 : T1 <: F1,
        t2 : T2 <: F2,
    Γ, u : U |- k : well-typed
=>
    Γ |- [record-init[U] [t1 t2] [u] [k]] : well-typed
```

### フィールド参照

F, U を型とし、T をレコード型とし、f をそのフィールドとするとき、次が成り立つ。(T, U: ?Sized)

```
    |- f : F,       // フィールドの型
    Γ |- t : *T,
    Γ, u : *U |- k : well-typed
=>
    Γ |- [field-ref [t f] [u] [k]] : well-typed
```

`mut` の方も同様。

### 和・差

ポインタと整数の和:

```
    Γ |-
        t1 : *T1,
        t2 : isize,     // or usize
    Γ, u : *T1 |- k : well-typed
=>
    Γ |- [add [t1 t2] [u] [k]] : well-typed
```

差も同様。

整数 + 整数: T を整数型、浮動小数点数型または文字型とするとき、次が成り立つ。

```
    Γ |-
        t1 : T,
        t2 : T,
    Γ, u : T |- k : well-typed
=>
    Γ |- [add [t1 t2] [u] [k]] : well-typed
```

TODO: おそらくこのタイミングでリテラルや定数に由来する併合型を消去する。例えば (x : i32) + (1 : iNN) なら結果は i32 にしつつ 1 : i32 に変更する。

積も同様。

### 積など

WIP

### ビット演算

WIP

### 比較

WIP

### 代入

```
    Γ |-
        t1 : *mut T,
        t2 : S <: T,
    Γ |- k : well-typed
=>
    Γ |- [assign [t1 t2] [] [k]] : well-typed
```

### if

```
    Γ |-
        cond : bool,
        k1 : well-typed,
        k2 : well-typed
=>
    Γ |- [if [cond] [] [k1 k2]] : well-typed
```

### ...

WIP
