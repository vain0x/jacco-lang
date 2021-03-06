# CPS 変換

WIP

式の構文木を CPS 中間表現に変換する処理を CPS 変換という。

CPS 変換は関数の内部や定数の初期化式など、実行コードの書かれる箇所ごとに独立して行う。

CPS 変換は一般的にクロージャを使って式の「残りの部分」を持ちながら作っていく。一方、いまの実装ではフラットな命令列 (レジスタマシンのバイトコードのようなもの) を構築して、それ木構造に畳み込むような手法を取っている。(Rust の所有権システムとクロージャは相性が悪いため。)

## CPS ノード

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

## 変換例

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

- [コンパイラの作り方 (詳解)](https://www.is.s.u-tokyo.ac.jp/vu/96/cad/compilerresume/)
- [RustでCPS変換が簡単になったよという話 | κeenのHappy Hacκing Blog](https://keens.github.io/blog/2019/12/07/rustdecpshenkangakantanninattayotoiuhanashi/)
- [なんでも継続](http://practical-scheme.net/docs/cont-j.html)
