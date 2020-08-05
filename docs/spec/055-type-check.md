# 型つけ規則

型検査は構文木ではなく CPS 中間表現の上で行う。

環境 Γ においてノードが型検査を通ることを次のように表す。

```
    Γ |- k : well-typed
```

CPS ノードは次のように表す。ただし型パラメータのリスト `[tys...]` や継続のリスト `[conts...]` は空のとき省略する。

```
    [kind[tys...] [args...] [results...] [conts...]]
```

型は、特に明記しなければ Sized (静的に型が確定する型) に制限される。

## `()`, true, false の型つけ規則

```
    Γ |-
        () : (),
        true : bool,
        false : bool.
```

## 数値リテラルの型つけ規則

数値リテラルの型は以下のように決める:

- 型サフィックスを持つなら、そのサフィックスが指定する型である。例: `42_i32 : i32`
- 値が i64, u64 のどちらでも厳密に表現できないなら、fNN である。例: `3.14 : fNN`
- 値が u64 で表現できないなら、iNN である。例: `-1 : iNN`
- いずれでもなければ uNN である。例: `42 : uNN`

## 文字リテラルの型つけ規則

文字リテラルの型は以下のように決める:

- 型サフィックスを持つなら、そのサフィックスが指定する型である。例: `'a'_c8 : c8`
- そうでなければ cNN である。

## 文字列リテラルの型つけ規則

TBD

## ノードの型つけ規則

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

引数が2個で継続が1個のケース:

f, t1, t2 を項とし、T1, T2, S1, S2 を型とし、k をノードとするとき、次が成り立つ。

```
    Γ |-
        f : fn(T1, T2) -> !,
        t1 : S1 <: T1,
        t2 : S2 <: T2,
        k : well-typed
=>
    Γ |- [jump [f t1 t2] [] [k]] : well-typed
```

### 関数呼び出し

引数が2個のケース:

```
    Γ |-
        f : fn(T1, T2) -> U,
        t1 : S1 <: T1,
        t2 : S2 <: T2,
    Γ, u : U |- k : well-typed
=>
    Γ |- [call-direct [f a1 a2] [u] [k]] : well-typed
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

```
    (TODO: t1, t2, ... にそれぞれ T の 1, 2, ... 番目のフィールドの型がつくこと),
    Γ, u : U |- k : well-typed
=>
    Γ |- [record-init[U] [t1 t2 ...] [u] [k]] : well-typed
```

### フィールド参照

型 T がラベル l のフィールドを持っていて、その型を U とするとき:

```
    Γ |-
        T : ?Sized,
        U : ?Sized,
        t : *T,
    Γ, u : *U |- k : well-typed
=>
    Γ |- [field-ref [t l] [u] [k]] : well-typed
```

`mut` の方も同様。

### 和 (ポインタ + 整数)

```
    Γ |-
        t1 : *T1,
        t2 : T2 <: (iNN | uNN),
    Γ, u : *T1 |- k : well-typed
=>
    Γ |- [add [t1 t2] [u] [k]] : well-typed
```

### 数値の和

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

TODO: 本体の型の min をとる操作をどこかに埋めないとまずい気がする

### ...

WIP
