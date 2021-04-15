# 例外

WIP

目的:

- 統一的なエラー処理の方法を規定する

当面、予定する機能:

- 例外の送出・再送は明示的な構文を持つ
- 例外をキャッチして値に変換できる
- エラーは出力引数を介して受け渡される
- エラートレースを自動で構築する
- 関数が例外を投げうるか否かはシグネチャに規定される

## エラー処理の基本戦略

エラーを以下の4種類に大きく分類する。

- 論理エラー: プログラムの不具合によるエラー。コードを変更して修正すべきもの。配列の範囲外参照など。
- 偏在エラー: プログラムのあらゆる箇所で発生する可能性があるエラー。スタックオーバーフローなど。
- 計算エラー: 計算結果が定義されない(エラー状態になる)ことによるエラー。文字列のパースの失敗など。
- 環境エラー: プログラムの実行環境に起因するエラー。実装時点で防ぐことはできないので、実行時に処理する必要がある。ファイルがないとか。

計算エラーは単なる「結果がOption/Resultであるような計算」ともいえるが、「環境エラー」と合成できると嬉しいので、エラーとして扱う。

エラーからの復帰に関しては以下の通り。

- 論理エラー: 復帰できないので、クラッシュさせる。デバッグのためスタックトレースを出したい。
- 偏在エラー: 復帰は難しいのでクラッシュさせる。
- 計算エラー: 頻繁に発生して、容易に復帰できる。エラーの発生・復帰は高速であるべき。
- 環境エラー: あまり発生しない。復帰することもあるが、しないこともよくある。高速でなくてもいい。

### エラー処理の典型

- エラーを発生させる。(throw)
- エラーをそのまま伝播する。(rethrow)
    - エラーの型は拡大することがある。
    - エラーに追加の情報 (コンテキスト) を付与したいことがある。(expect)
- エラーを他のオブジェクトで包んで、伝播する。(wrap)
- エラーを無視して、既定値を計算する。(coalesce)
- エラーを値として取り出す:
    - 値として取り出したエラーを伝播する。(catch)
    - 値として取り出したエラーやスタックトレースを出力する。(debug)
    - 値として取り出したエラーを、スタックトレースを維持しながら伝播する。(rethrow)
- エラーが起こったとき、リソースを解放する。(err-defer)
- エラーが起こったときも起こらなかったときも、リソースを解放する。(finally)

## エラー処理の言語機能

式は、評価して結果を返すのではなく、例外を投げることができる。
例外はエラーを表現する何らかのオブジェクト。

### 例外の型指定

関数が例外を投げるかどうかは関数の型の一部である。
関数を定義する際に、throws キーワードを使って、その関数が投げる可能性のある例外の型を指定する。

```rust
/// ファイルの中身を読み込む。読み込めなかったら ReadFileError 型の例外を投げる。
fn read_file(path: *c8, buf: *mut u8) throws ReadFileError {
    // ...
}
```

### 例外の送出

throw 式は例外を投げる。

```rust
    throw expr;
```

throw 式を含む handle 式がある場合、その handle 式が例外を処理する。
そうでなければ、関数が呼び出し元に例外を投げる。

```rust
struct ReadFileError;

fn read_file(path: *c8, buf: *mut u8) throws ReadFileError {
    let file = fopen(...);
    if is_null(file) {
        // 関数から例外で抜ける。
        throw ReadFileError;
    }

    fread(...);
}
```

## 例外の処理

例外を処理するには handle 式を使う。
`expr.handle` 式は `expr` が投げた例外を処理する。
つまり、その例外に関してパターンマッチを行い、マッチした節の結果を返す。
`expr` が例外を投げなかった場合は、その結果を返す。

```rust
    operation().handle {
        err => {
            // err を使う処理
        }
    }
```

節の内部では recover 式を使って handle 式から脱出できる。
`recover expr` は `expr` を評価し、その結果を handle 式の結果とする。
評価が節の末尾に達したときは err を再送する。

```rust
    Ok(operation()).handle {
        err if err.is_canceled() => recover Canceled,
        err if err.is_timeout() => recover Timeout,
    }
```

### 典型的な例外処理

典型的なユースケースのため、handle 式は以下の糖衣構文を持つ。

try 式はエラーを伝播する。

```rust
    expr.try
// =>
    expr.handle { _ => {} }
```

再送に際して、追加の情報を与えることもできる。

```rust
    expr.try(context)
// =>
    expr.handle { err => err.set_context(context) }
```

catch 式はエラーを値として取り出す。

```rust
    expr.catch
// =>
    Ok(expr).handle { err => recover Err(err) }
```

or 式はエラーを既定値に変換する。

```rust
// '(' が後続するとき。例外にはアクセスできない。
    expr.or(alt)
// =>
    expr.handle { _ => recover alt }
```

```rust
// '{' が後続するとき。
    expr.or { err => alt(err) }
// =>
    expr.handle { err => recover alt(err) }
```

### defer 文

defer 文の後にある、同じブロックの文や式より後に処理を挟む。
この処理はそのブロックの評価が完了したとき、外にジャンプしたとき、例外が投げられたときのいずれも行われる。

defer は handle 式ではないので、例外を処理したとはみなさない。

## 例外の表現

### std::err::Try 型

```rust
// 例外処理の結果
enum Try[T, E] {
    Ok {
        value: T,
    },
    Err {
        trace: ErrTrace,
        data: *E,
    },
}

// Try[_, E]::Err と同じメモリレイアウトを持つ。(コンパイラが保証する。)
struct TryErr[E] {
    tag: u8,
    trace: ErrTrace,
    data: *E,
}

// 例外が送出・再送された位置、付与された情報のリスト
type ErrTrace = Vec[ErrPoint];

struct ErrPoint {
    // 例外処理の位置情報
    location: SourceLocation,

    // 付与された情報
    context: *unknown,
}
```

### 例外の送出の挙動

main 関数は例外を投げないので、例外は必ずどこかで「常に recover する handle 式」に処理される。そのため例外を投げる関数の一連の呼び出しは1個の Try オブジェクトを生成する。

関数が例外を投げずに return したときは、その値で Ok(x) を構築する。例外が投げられる場合、投げられた値を Err バリアントに持たせる。当面は、例外を投げる関数に暗黙の出力引数として Try オブジェクトへのポインタを渡すことにして、throw に際して Err オブジェクトを初期化することにする。

throw とその後の再送の際にエラートレースに位置情報を書き込んでいく。その書き込み先のメモリ領域は、recover を含む handle 式が事前に確保しておくことにする。throw の際に確保すると、それを解放する責任をコンパイル時に決定できないため、動的確保を行うしかなくなる。recover 側ならスタック上にメモリを確保できて効率がいいはず。

handle 側で例外のエラートレースが捨てられることが分かっている場合はエラートレースを長さ 0 で確保することにより、エラートレースの生成を抑制する。(エラーを起こす単純な式を効率的に処理するため。)

例:

```rust
struct ReadFileError;

fn read_file(path: *c8, buf: *mut u8) throws ReadFileError {
    let file = fopen(...);
    if is_null(file) {
        // 関数から例外で抜ける。
        throw ReadFileError;
    }

    fread(...);
}

let result = read_file("foo.txt", &mut buf).catch;
```

これはおおよそ次のような挙動になる (あまり厳密には決まっていない):

```rust
fn read_file(path: *c8, buf: *mut u8, err: *mut TryErr[/* TBD */]) throws ReadFileError {
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

// let result = (...).catch;
let result = Try::[u32, /* TBD */]::Err {
    trace: ErrorTrace::stack_alloc(MAX_TRACE_LEN), // エラートレースの領域は事前に確保する。
    data: uninit,
};
let value = read_file("foo.txt", &mut buf, &mut result);
if result.tag != Try::Err::tag {
    result.value = value;
}
```

### 例外の再送の挙動

関数が例外を投げたか検査して、投げられていたらエラートレースに追記して return。投げられていなければ続行。

```rust
fn cat(path1: *c8, path2: *c8, buf: *mut u8) throws ReadFileError {
    read_file(path1, buf).try;
    read_file(path2, buf).try
}
```

おおよそ次のような挙動:

```rust
fn cat(path1: *c8, path2: *c8, buf: *mut u8, err: *mut TryErr[ /* TBD */ ]) throws ReadFileError {
    // read_file(path1, buf).try;
    read_file(path1, buf, err);
    if (*err).tag == Try::Err::tag {
        (*err).trace.push(TraceItem { ... }); // .try の位置情報
        return uninit;
    }

    read_file(path2, buf).try
}
```

### 例外のキャッチとハンドル

TBD

```rust
enum IoError {
    NotFound,
    PermissionDenied,
    // ...
}

let result = read_file("not_found.txt", &mut buf).catch;
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

## その他

handle の中でジャンプ:

```rust
    loop {
        let item = queue.pop().or(break);
    }
```

エラーの握りつぶし:

```rust
    f().or(unit);
```

handle 箇所はエラートレースを確保するので、アロケータを持つ必要がある。

## 課題

- TODO: キャッチしたエラーに基づく処理 (ダウンキャストなど)
- TODO: ループの中の defer の扱い
- TODO: catch したエラーを throw するときエラートレースをコピーする必要がある
- TODO: リソース管理との相互作用 (例外安全性など)
- TODO: 非同期処理との相互作用
- TODO: パターンマッチに失敗したときにエラーを起こす

## 調査メモ

Swift のエラーハンドリングのガイド:

- [Error Handling — The Swift Programming Language (Swift 5.3)](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Error Handling Rationale and Proposal](https://github.com/apple/swift/blob/7123d2614b5f222d03b3762cb110d27a9dd98e24/docs/ErrorHandlingRationale.rst)

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
