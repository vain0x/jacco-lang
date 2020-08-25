# ジェネリクス

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

関数の宣言は型引数を持てる。

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

```rust
// スライスのバブルソート

fn bubble_sort[T](
    ptr: *mut T,
    len: usize,
    // T のサイズ (不正な値を渡すと undefined behavior)
    value_size: usize,
    // T の比較関数へのポインタ (型安全)
    compare_fn: fn(*T, *T) -> i32
) {
    // コンパイラは T のサイズを知らないので &ptr[i] や ptr += i は書けない。
    // 代わりに usize でアドレスを計算して、必要に応じてポインタに再キャストする。

    let p = ptr as usize;
    let r = p + value_size * len;
    while p + value_size < r {
        let q = p + value_size;
        while q < r {
            if compare_fn(p as *T, q as *T) > 0 {
                swap(p as *mut T, q as *mut T, value_size);
            }
            q += value_size;
        }
        p += value_size;
    }
}

fn swap[T](left: *mut T, right: *mut T, size: usize) {
    let t = [0_u8; size] as *mut T;
    memcpy(t, left, size);
    memcpy(left, right, size);
    memcpy(right, t, size);
}
```

- [ベクタ](../../libjacco_alloc/src/vec.jacco)

## 今後の拡張の予定

- enum, struct などの型も型変数を持てるようにする
- 使いやすくする案
    - 暗黙的に型のメモリレイアウトを引数で渡す (?)
    - 辞書渡し (?)
    - 単相化 (?)

ほか
