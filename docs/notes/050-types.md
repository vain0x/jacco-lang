# 型の各論

WIP

型の種類や、型がとりうる値、適用可能な操作などを定める。

## ユニット型

unit は型であり、unit 型の値を指す定数としても使える。unit 型の値は unit の1つに限られる。この値のサイズは 0 ビットであり、情報を持たないことが保証される。unit は bool の小さいバージョンだと思える。bool は2個の値を持ち、1ビットの情報を表す。

unit を含めて、サイズが 0 ビットの型を **ゼロサイズ型** と総称する。

## bool 型

bool は型であり、値は定数 true, false の2つに限られる。この値のサイズは 1 バイトである。

同値と順序の比較演算と、論理演算が定まる。順序は `false < true`。

bool 型は符号つき整数型、符号なし整数型と相互に変換できる:

- bool → 整数:
    - false → 0
    - true → 1
- 整数 → bool:
    - 0 → false
    - 0 以外 → true

## 符号つき整数型

組み込みの符号つき整数型は固定のビット幅を持ち、2の補数で表現される。isize のビット幅はコンパイル対象によって決まり、ポインタのビット幅に等しい。

| 名前   | ビット幅   | 意味    |
|:------|----------:|:--------------|
| i8    | 8         | 符号つき整数  |
| i16   | 16        | 符号つき整数  |
| i32   | 32        | 符号つき整数  |
| i64   | 64        | 符号つき整数  |
| isize | ptr       | 符号つき整数  |

符号つき整数型を総称して iNN と書く。

符号つき整数型は互いに部分型関係を持たない。isize は、もしビット幅が一致しても i32 や i64 とは異なる型であり、部分型関係を持たない。

算術演算、ビット演算、同値および順序の比較演算が定まる。

数値型 (符号つき整数型、符号なし整数型、浮動小数点数型、文字型) の値は、as 演算子により相互に変換できる。

isize はポインタ型との和・差の右辺になれる。添字式の中に書ける。

TODO: i128, u128 を追加

## 符号なし整数型

組み込みの符号なし整数型は固定のビット幅を持つ。usize のビット幅は isize と同じくポインタの幅に等しい。

| 名前   | ビット幅   | 意味    |
|:------|----------:|:--------------|
| u8    | 8         | 符号なし整数  |
| u16   | 16        | 符号なし整数  |
| u32   | 32        | 符号なし整数  |
| u64   | 64        | 符号なし整数  |
| usize | ptr       | 符号なし整数  |

符号なし整数型を総称して uNN と書く。

算術演算、ビット演算、同値および順序の比較演算が定まる。

isize と同様に、usize は as 演算子により数値型やポインタ型と相互に変換でき、ポインタの和・差の右辺になれて、添字式の中に書ける。

## 浮動小数点数型

浮動小数点数型は固定のビット幅を持つ。値の表現は IEEE-754 により定義される。

| 名前   | ビット幅   | 意味    |
|:------|----------:|:--------------|
| f32   | 32        | 浮動小数点数  |
| f64   | 64        | 浮動小数点数  |

浮動小数点数型を総称して fNN と書く。

算術演算、同値および順序の比較演算が定まる。

浮動小数点数型の値は as 演算子により相互に変換でき、

TODO: f16, f128 を追加

## 文字型

文字型 c8, c16, c32 の実行時の表現は、それぞれ u8, u16, u32 に等しい。

| 名前   | ビット幅   | 意味    |
|:------|----------:|:----------------|
| c8    | 8         | UTF-8 コードユニット  |
| c16   | 16        | UTF-16 コードユニット |
| c32   | 32        | UTF-32 コードユニット |

文字型を総称して cNN と書く。

注意: 文字型の値が所定のエンコーディングのコードユニットとして有効かどうかはコンパイル時にも実行時にも検査されないし、違反してもプログラムの誤りとはみなされない。文字型の配列が所定のエンコーディングの文字列として有効かどうかも同様に、違反しても誤りとはみなされない。

## 特殊な型: unknown

unknown は型である。値のサイズは静的に決定できない (不定サイズの型)。

T を型とするとき、`*T` は `*unknown` の部分型である。`*T` と `*unknown` は相互に変換できる。

`*unknown` はC言語の `const void*` に相当し、`*mut unknown` は `void*` に相当する。

## 特殊な型: never

never は型であり、値を持たない。

never 型の式を評価するとき、「評価が停止して何らかの値が得られる」ことはない。代わりに、無限ループに陥って停止しない、関数から return する、プログラム自体が終了する、などの現象が起こる。

never はあらゆる型の部分型であり (`never <: T`)、never 型の式は他の型の変数や引数に代入する式の型検査が通る (e.g. `let x: i32 = abort();`)。(前述の通り、実際には代入に到達しないので問題ない。)

関連記事:

- [Never patterns, exhaustive matching, and uninhabited types (oh my!)](https://smallcultfollowing.com/babysteps/blog/2018/08/13/never-patterns-exhaustive-matching-and-uninhabited-types-oh-my/)

## 読み取り専用のポインタ型

T を型とする。`*T` は読み取り専用のポインタ型である。`*T` は `*const T` とも書ける。

型 T に関する部分型関係は共変 (covariant) に遺伝する。

## 可変なポインタ型

T を型とする。`*mut T` は可変なポインタ型である。

型 T に関する部分型関係は遺伝しない (反変である)。`*mut T <: *T` (可変性の忘却) と `*mut T <: *mut unknown` (unknown への昇格) の部分型関係を持つ。

備考: `*mut T` のような読み書き可能な参照を共変・反変にすると、例えば `Cat <: Animal, Dog <: Animal` のような部分型関係があるとき `cat: *mut Cat` に対して `*(cat as *mut Animal) = dog as Animal` のような整合性のない書き込みを許容してしまう。例外的に、`*mut unknown` の参照先への書き込みは (型システム的には) 常に unsafe なので、`*mut unknown` への昇格を禁止する理由はない (はず)。

## enum 型

enum 文 `enum T ...` は新しい型 T を導入する。これを enum 型と呼ぶ。

enum 型は構文的に次の2つの種類に分類できる:

- 定数バリアント (型注釈または初期化式を持つバリアント) を含むとき、const enum　という。
- 定数バリアントを含まないのとき、struct enum という。

ただし次のようなバリアントは不正である:

- 定数バリアントとレコードバリアントが混在するような enum 文は不正である。
- バリアントが1つもない enum は不正である。(もし定義できたら、それはインスタンスを持たないので never と同様であり、型システムが複雑化してしまう。)

### const enum 型の表現

const enum 型は数値型 (iNN/uNN/fNN/cNN) のいずれかと同じ表現を持つ。

定数バリアントが持つ型注釈と初期化式はすべて一貫した型を持つ必要があり、その型を const enum の表現型という。const enum 型の実行時の表現は表現型と同じになる。

### struct enum 型の表現

タグつきユニオンとして表現される。

タグの型は (バリアントの個数 + 1) を表現できる最小の符号なし整数型 (uNN) になる。構文的に i 番目に出現するバリアントのタグの値は i である (i = 0 から数える)。

例:

```rust
struct I32Option {
    None,
    Some { value: i32 },
}
```

変換 =>

```c
struct I32Option_Some {
    int32_t value;
}

struct I32Option {
    uint8_t tag;

    union {
        I32Option_Some Some;
    };
};
```

TODO: レコード式、フィールド参照式、match

## struct 型

struct 文 `struct T ...` は新しい型 T を導入する。これを struct 型と呼ぶ。

実行時の表現はC言語の struct と同じ。

フィールドの型として unit, never 型は指定できない。

TODO: ジェネリクス, unit-like struct, record struct, レコード式, フィールド参照式

## 関連記事

- C言語の算術型: [Arithmetic types](https://en.cppreference.com/w/c/language/arithmetic_types)
- C言語の標準ライブラリの型: [Type support](https://en.cppreference.com/w/c/types)
