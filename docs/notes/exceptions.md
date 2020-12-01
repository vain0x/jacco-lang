# 例外

WIP

目的:

- 統一的なエラーハンドリングの方法を規定する

当面、予定する機能:

- 例外の送出・再送は明示的な構文を持つ
- 例外をキャッチして値に変換できる
- エラーは出力引数を介して受け渡される
- エラートレースを自動で構築する
- 関数が例外を投げうるか否かはシグネチャに規定される

## エラー処理の基本戦略

例外は、関数の事前条件を満たしていても発生する可能性があるエラーの処理に使用する。ネットワークエラーなど。

関数の事前条件を満たしているときには発生しないエラーの処理には、例外を使用しない。代わりに次を行う:

- abort で落とす。
    - 例: 配列の不正なインデックスへのアクセス
- undefined behavior とみなす。
    - 例: 不正なポインタへのアクセス

## 例外を投げる関数の宣言

throws キーワードのついた関数文は例外を投げる関数を宣言する。

```rust
/// ファイルの中身を読み込む。読み込めなかったら例外を投げる
fn read_file(path: *c8, buf: *mut u8) throws {
    // ...
}
```

## 例外の送出

`throw e` は例外 e を投げて、外側の try ブロックまたは関数から例外を伴って抜ける。どの関数やブロックから抜けるかは、break/return と同様に構文から確定する。

```rust
struct ReadFileError;

fn read_file(path: *c8, buf: *mut u8) throws {
    let file = fopen(...);
    if is_null(file) {
        // 関数から例外で抜ける。
        throw ReadFileError;
    }

    fread(...);
}
```

## 例外の再送

例外を投げる関数の呼び出しには `.try` をつける。また、try ブロックか例外を投げる関数の中にしか書けない。

```rust
fn cat(path1: *c8, path2: *c8, buf: *mut u8) throws {
    read_file(path1).try;
    read_file(path2).try;
}
```

## 例外のキャッチ

`try { ... }` は内側で投げられた例外をキャッチする。正常に抜けたときは Ok バリアント、例外で抜けたときは Err バリアントになる。

```rust
fn main() -> i32 {
    let buf = ...;
    let ok = try { read_file("available.txt", &mut buf) }; //=> Ok(...)
    let bad = try { read_file("not_found.txt", &mut buf) }; //=> Err(...)

    if ok.is_ok() && bad.is_ok() { 0 } else { 1 }
}
```

## std::err::Try 型

```rust
// try { ... } の結果
enum Try[T, E] {
    Ok {
        value: T,
    },
    Err {
        trace: ErrorTrace,
        data: E,
    },
}

// Try::Err と同じメモリレイアウトを持つ。
struct TryErr[E] {
    tag: u8,
    trace: ErrorTrace,
    data: E,
}

// 例外が送出・再送された位置のリスト
type ErrorTrace = Vec[SourceLocation];
```

## 例外の送出の挙動

main 関数は例外を再送しないので、例外は必ずどこかで try ブロックにキャッチされる。そのため例外を投げる関数の一連の呼び出しは1個の Try オブジェクトを生成する。

関数が例外が投げずに return したときは、その値で Ok(x) を構築すればいい。例外が投げられる場合、投げられた値を Err バリアントに持たせる必要がある。当面は、例外を投げる関数に暗黙の出力引数として Try オブジェクトへのポインタを渡すことにして、throw に際して Err オブジェクトを初期化することにする。

throw とその後の再送の際にエラートレースに位置情報を書き込んでいく。その書き込み先のメモリ領域は、try ブロック側が事前に確保しておくことにする。throw の際に確保すると、それを解放する責任をコンパイル時に決定できないため、動的確保を行うしかなくなる。try ブロックならスタック上にメモリを確保できて効率がいいはず。

例:

```rust
struct ReadFileError;

fn read_file(path: *c8, buf: *mut u8) throws {
    let file = fopen(...);
    if is_null(file) {
        // 関数から例外で抜ける。
        throw ReadFileError;
    }

    fread(...);
}

let result = try { read_file("foo.txt", &mut buf) };
```

これはおおよそ次のような挙動になる (あまり厳密には決まっていない):

```rust
fn read_file(path: *c8, buf: *mut u8, err: *mut TryErr[/* TBD */]) throws {
    let file = fopen(...);
    if is_null(file) {
        // throw ...;
        (*err).tag = Try::Err::tag;
        (*err).trace.push(TraceItem { ... }); // throw の位置情報
        (*err).data = ReadFileError;
        return uninit; // 結果は使用されない
    }

    fread(...);
}

// let result = try { ... };
let result = Try::[u32, /* TBD */]::Err {
    trace: ErrorTrace::stack_alloc(MAX_TRACE_LEN), // エラートレースの領域は事前に確保する。
    data: uninit,
};
let value = read_file("foo.txt", &mut buf, &mut result);
if result.tag != Try::Err::tag {
    result.value = value;
}
```

## 例外の再送の挙動

関数が例外を投げたかチェックして、投げられていたらエラートレースに追記して return。投げられていなければ続行。

```rust
fn cat(path1: *c8, path2: *c8, buf: *mut u8) throws {
    read_file(path1, buf).try;
    read_file(path2, buf).try
}
```

おおよそ次のような挙動:

```rust
fn cat(path1: *c8, path2: *c8, buf: *mut u8, err: *mut TryErr[ /* TBD */ ]) throws {
    // read_file(path1, buf).try;
    read_file(path1, buf, err);
    if (*err).tag == Try::Err::tag {
        (*err).trace.push(TraceItem { ... }); // .try の位置情報
        return uninit;
    }

    read_file(path2, buf).try
}
```

## 例外のキャッチとハンドル

TBD

```rust
enum IoError {
    NotFound,
    PermissionDenied,
    // ...
}

let result = try { read_file("not_found.txt", &mut buf).try };
match result {
    Try::Ok { .. } => {}
    Try::Err {
        trace: _,
        data: *data, // data: *unknown
    } => {
        // TODO: throws にエラーの型を指定しないなら安全なダウンキャスト機構が必須
        match *(data as *IoError) {
            IoError::NotFound => {}
            // ...
        }
    }
}
```

## 課題

- TODO: throws にエラーの型を厳密に指定できるか? (検査例外)
- TODO: キャッチしたエラーに基づく処理 (ダウンキャストなど)
- TODO: ジャンプ命令のフック (return/throw の片方または両方で抜ける直前に実行するコードを書く構文)
- TODO: エラーをラップして再送したいケース
- TODO: エラーにコンテクスト (デバッグのヒントになる文字列や追加のデータなど) を付与して再送したいケース
- TODO: try ブロックでつかんだエラーを改めて throw するとエラートレースが消える問題
- TODO: 関数が例外を投げた際に return ではなく continue してループが終わってから throw したいケース (?)
- TODO: エラーが発生した後に実行を継続したいケース (?)
- TODO: リソース管理との相互作用 (例外安全性など)
- TODO: 非同期処理との相互作用

## 調査メモ

Swift のエラーハンドリングのガイド:

- [Error Handling — The Swift Programming Language (Swift 5.3)](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)

C++ に静的例外を導入する提案:

- [P0709 Zero-overhead deterministic exceptions\: Throwing values · Issue #310 · cplusplus/papers](https://github.com/cplusplus/papers/issues/310)
    - 批判の1つ: [Reply to Zero-overhead deterministic exceptions](https://lists.isocpp.org/std-proposals/att-0486/Reply_to_Zero-overhead_deterministic_exceptions:_Throwing.pdf)
    - 日本語での解説: [C++に提案されている静的例外](https://cpplover.blogspot.com/2018/07/c.html)
    - この提案を受けてビャーネ氏が書いたという記事の和訳: [［翻訳］なぜそんなに確信が持てるのか？](https://onihusube.hatenablog.com/entry/2019/12/13/211603)
    - [The Evils of ParadigmsOrBeware of one-solution-fits-all thinking | p0976r0.pdf](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0976r0.pdf)
- [Low-Cost Deterministic C++ Exceptions for Embedded Systems](https://www.research.ed.ac.uk/portal/files/78829292/low_cost_deterministic_C_exceptions_for_embedded_systems.pdf)

Java の検査例外に関する記事:

- [検査例外に対する批判](https://scrapbox.io/tasuwo/%E6%A4%9C%E6%9F%BB%E4%BE%8B%E5%A4%96%E3%81%AB%E5%AF%BE%E3%81%99%E3%82%8B%E6%89%B9%E5%88%A4)
- [Javaの検査例外の欠点について](https://kmizu.hatenablog.com/entry/20100111/1263225681)

Java の検査例外の悪い点 (?):

- 関数のシグネチャが過剰に詳細になる:
    - 例外中立な関数に例外型を列挙するのは時間の無駄
    - 例外型の列挙は手間がかかるのに人々はそれを無駄にやりたがる。実際には一括でハンドルされてたりする
    - ある関数が投げるエラーが増えたとき、それを呼び出す関数にも検査例外を追加する必要がある (モナド汚染に似てる)
- 高階関数と相性が悪い:
    - エフェクトに関する抽象が必要 (?)
    - 参考: [The problem of effects in Rust](https://without.boats/blog/the-problem-of-effects/)
- 再送の構文が冗長

エラーハンドリングに関連する記事:

- [Working with Errors in Go 1.13 - The Go Blog](https://blog.golang.org/go1.13-errors)
