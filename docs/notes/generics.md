# ジェネリクス

WIP

当面の目標:

- 多相性: 特定の型に依存しないデータ構造やアルゴリズムを記述するための最低限の能力を提供する (パラメトリック多相)
- 抽象性: 関数の本体が使用箇所に影響しないこと (C++ のテンプレートは NG)

目的外:

- ゼロオーバーヘッドの抽象化
- 型推論による暗黙の多相化 (let 多相性など)
- アドホック多相 (トレイト境界など)
- 実行時の多相 (トレイトオブジェクトなど)
- 型レベル計算、コンパイル時計算など

## 型変数

関数文は型引数を持てる。

```rust
fn f[T](...) { ... }
//  ^^^ 型引数リスト
```

型引数を持つシンボルは、そのシグネチャや本体において、その型引数を型変数として参照できる。

```rust
fn as_const[T](p: *mut T) -> *T {
//                     ^      ^  シグネチャに出現する型変数
    p as *T
//        ^ 本体に出現する型変数
}
```

ただし入れ子に宣言されている他のシンボルの中で、その型変数を使うことはできない。

```rust
fn outer[T]() {
    fn inner(p: *T) {
        //       ^ NG: 型変数 T は outer で定義されているので、
        //             inner のシグネチャや本体には出現できない
    }
}
```

## 多相型の型検査

多相なシンボルの式には、シンボルの型引数にそれぞれフレッシュなメタ型変数を代入することで得られる型がつく。(いわゆる多相型のインスタンス化。)

```rust
    let p = as_const(1 as *mut c8);
    //      ^^^^^^^^ fn(*mut a) -> *a (a = c8)

    let q = as_const(4 as *mut i32);
    //      ^^^^^^^^ fn(*mut b) -> *b (b = i32)
```

## 多相型のコード生成

型変数のコード生成における振る舞いは unknown 型と同じ。(いわゆる型消去意味論。)

例:

```rust
fn as_const[T](p: *mut T) -> *T {
    p as *T
}

let p = as_const(1 as *mut c8);
```

変換 =>

```c
void const* as_const(void* p) {
    return (void const*)p;
}

char const* p = as_const((char*)1);
```

## 型変数の制限

当面、型変数に何らかの性質を要求するコードを書くことはできない。

- 型変数が表す型のサイズやアラインメントはとれない。(`sizeof T` に相当する操作はない。)
- メモリレイアウトが型変数に依存する値はコピーできない。
- 加算演算子などが式の型に一定の条件を課すとき、型変数はその条件を満たさない。

```rust
// ✔ OK

fn as_const[T](x: *mut T) -> *T {
//             ^ OK: ポインタ型の受け渡しは T に依存しない
    x as *T
//    ^^ OK: *mut T → *T のキャストは T によらず可能
}
```

```rust
// ✗ NG

fn id[T](x: T) -> T {
//       ^ NG: 型変数 T に依存している値を引数にとることはできない
    x
//  ^ NG: 型変数 T に依存している値を関数から返すことはできない
}
```

## 例

型に依存しないアルゴリズムを書ける。

```rust
// スライスのバブルソート

fn bubble_sort[T](
    ptr: *mut T,
    len: usize,
    /// T のサイズ (不正な値を渡すと undefined behavior)
    value_size: usize,
    /// T の比較関数へのポインタ (型安全)
    compare_fn: fn(*T, *T) -> i32,
) {
    let i = 0_usize;
    while i < len {
        let j = i + 1;
        while j < r {
            let p = slice_index_mut(ptr, i, value_size);
            let q = slice_index_mut(ptr, j, value_size);
            if compare_fn(p, q) > 0 {
                swap(p, q, value_size);
            }
            j += 1;
        }
        i += 1;
    }
}

/// ptr から始まる可変スライスの index 番目へのポインタを得る。
fn slice_index_mut[T](ptr: *mut T, index: usize, value_size: usize) -> *mut T {
    // コンパイラは T のサイズを知らないので &ptr[i] とは書けない。
    (ptr as usize + index * value_size) as *mut T
}

fn swap[T](left: *mut T, right: *mut T, size: usize) {
    let t = [0_u8; size] as *mut T; // 配列は未実装
    memcpy(t, left, size);
    memcpy(left, right, size);
    memcpy(right, t, size);
}

```rust
// 使用例

fn use_bubble_sort() {
    let array: []i32 = [3, 1, 4, 1, 5]; // 配列は未実装
    let array_len = 5_usize;
    let i32_size = 4_usize;
    bubble_sort(&mut array, array_len, i32_size, i32_compare);
    // array == [1, 1, 3, 4, 5]
}

fn i32_compare(first: *i32, second: *i32) -> i32 {
    if *first < *second {
        -1
    } else if *first == *second {
        0
    } else {
        1
    }
}
```

- [ベクタ](../../libjacco_alloc/src/vec.jacco)

## 拡張予定: ジェネリックな構造体

fn と同様に struct も型引数を持てて、本体で型変数 T を使える。

```rust
struct Slice[T] {
    //      ^^^ 型引数リスト
    ptr: *T,
    //    ^ 型変数
    len: usize,
}

fn Slice_new[T](ptr: *T, len: usize) -> Slice[T] {
    //                                  ^^^^^^^^ 型引数をつける
    Slice {
        ptr: ptr,
        len: len,
    }
}

let hello: Slice[c8] = Slice_new("hello, world!", 5);

printf("%.*s", hello.ptr, hello.len);
//                  ^ フィールド参照
```

コード生成において、型変数に由来している型を脱参照する際はキャストを挟む必要がある。

```rust
let h: c8 = *hello.ptr;
//          ^ これの型は Slice の型変数に由来していて、
//            普通にコード生成すると void になってしまう
```

```c
char h = *(char const*)hello.ptr;
//        ^^^^^^^^^^^^^ 追加
```

メモリレイアウトが型変数に依存するジェネリックな構造体は不定サイズになる。不定サイズな構造体は値として受け渡すことはできず、レコード式で構築できない。フィールド参照はできる。

```rust
struct Option[T] {
    has_value: bool,
    value: T,
}

fn as_ref[T](opt: *Option[T]) -> Option[*T] {
    Option {
        has_value: (*opt).has_value,
        value: &(*opt).value,
    }
}
```

### 例

ジェネリックなデータ構造のためのライブラリを書けるようになる。また、アルゴリズムの実装も少し楽になる。

```rust
/// 型 T のメモリレイアウト。幽霊型として型変数を持つことで安全性が高まるはず。
struct Layout[T] {
    size: usize,
}

/// 可変なスライス。メモリ上の特定の型のオブジェクトがメモリ上に連続して配置されている領域への参照。
struct SliceMut[T] {
    ptr: *mut T,
    len: usize,
    layout: *Layout[T],
}

fn SliceMut_new[T](ptr: *mut T, len: usize, layout: *Layout[T]) -> SliceMut[T] {
    SliceMut {
        ptr: ptr,
        len: len,
        layout: layout,
    }
}

fn SliceMut_get[T](slice: SliceMut[T], index: usize) -> *mut T {
    assert(index < slice.len);

    // コンパイラは T のサイズを知らないので &ptr[index] とは書けない。
    ((slice.ptr as usize) + index * (*slice.layout).size) as *mut T
}
```

```rust
// スライスのバブルソート

fn bubble_sort[T](slice: SliceMut[T], ord: Ord[T]) {
    let i = 0_usize;
    while i < slice.len {
        let j = i + 1;
        while j < slice.len {
            let p = SliceMut_get(slice, i);
            let q = SliceMut_get(slice, j);
            if (ord.compare_fn)(p, q) > 0 {
                swap(p, q, slice.layout);
            }
            j += 1;
        }
        i += 1;
    }
}

struct Ord[T] {
    compare_fn: fn(*T, *T) -> i32,
}

fn swap[T](left: *mut T, right: *mut T, layout: *Layout[T]) {
    let size = (*layout).size;
    let t = [0_u8; size] as *mut T; // 配列は未実装
    memcpy(t, left, size);
    memcpy(left, right, size);
    memcpy(right, t, size);
}
```

```rust
// 使用例

fn use_bubble_sort() {
    static I32_LAYOUT = Layout::[i32] {
        size: 4,
    };
    static I32_ORD = Ord {
        compare_fn: i32_compare,
    };

    let array = [3, 1, 4, 1, 5]; // 配列は未実装
    bubble_sort(SliceMut_new(&mut array, 5), &I32_LAYOUT, &I32_ORD);
    // array == [1, 1, 3, 4, 5]
}

fn i32_compare(first: *i32, second: *i32) -> i32 {
    if *first < *second {
        -1
    } else if *first == *second {
        0
    } else {
        1
    }
}
```

## 今後の拡張の予定

- enum, struct などの型も型変数を持てるようにする
- パス式に型引数リストをつけられるようにする (`f::[i32]` など)
- 使いやすくする案
    - 暗黙的に型のメモリレイアウトを引数で渡す (?)
    - 辞書渡し (?)
    - 単相化 (?)
    - 固定長配列の長さに関するジェネリクス (?)

ほか
