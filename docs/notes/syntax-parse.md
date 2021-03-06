# 構文

WIP

ジャッコ言語の具象構文は Rust をベースとしていて、LL(2) 文法の範疇にある。

構文の要素の中心は式と文である。「式」は算術演算だけでなく、ブロック、条件分岐、反復などの制御構文も含む。一方で、「文」は主に変数や関数、型の宣言や定義など、それ自体は計算を表していないものが含まれる。ブロック式は文を含むので、式と文は相互再帰的に出現する。トップレベルは文の並びである。

式と文の他に、型やパターンの構文がある。これらは let 文などの内部で補助的に用いられている。対応する式の構文のサブセットに近い。

式や文の付加的な情報を記述するための「属性」の構文が存在する。(まだあまり実装されていない。)

## 形式的な構文の定義

[syntax.ungram](./syntax.ungram): ジャッコ言語の構文を BNF 風の形式言語 [ungrammar](https://github.com/rust-analyzer/ungrammar) で記述したもの。(注意: 処理系の実装と常には同期していない)

## 構文の分類

次のように分類できる:

- 属性
- パス: 識別子を `::` 区切りで並べたもの
- パターン
- 型
- 式
- 文

## 構文の設計に関するメモ

C言語の構文には問題がある (`#include`、マクロ、無限先読み、`t*p` 問題、ぶら下がり if、関数ポインタ型など)。Rust の構文はC言語に似ていつつも、多くの問題を解決している。そのため、ジャッコ言語は Rust の構文をベースにする。

おそらく ergonomics や既存言語との類似性を目的として、Rust にも解析を難しくする要素がある。ジャッコ言語は ergonomics より工学的なシンプルさを目指すため、これらの要素を変更して、~~context-free にする~~。当初は context-free にするという方針だったが、デメリットがあるのでやめることにした。

- ~~Rust は構造体リテラル `K {}` と制御構文 `if cond {}` などの構文が衝突していて、一部の式には構造体リテラルが出現できないことにして対処している。~~
    - ~~ジャッコ言語は制御構文の条件式をカッコを必須として `if (cond) {}` にする。~~
    - 追記: カッコが必須であることを忘れて `if p { ... }` と書いたとき `p { ... }` の部分は構造体リテラルとしてパースされることになり、適切でない構文エラーが報告されることが多かった。
- Rust は型引数のカッコに `<>` を使っていて、比較演算子やシフト演算子と弁別しづらい。
    - ジャッコ言語は `[]`/`::[]` を使う。`<` と `>` の対応を見るような構文は採用しない。
- Rust はブロック文の直後のセミコロンを省略するために、打ち切り規則を使っている (参考: [Rustの文でセミコロンを省略してよい条件](https://qnighy.hatenablog.com/entry/2017/04/22/070000))。
    - ジャッコ言語は構文規則を調整して対処する。(予定)
- Rust は強力なマクロ機能を持っていて、マクロを利用している部分は静的解析がしづらい。
    - ジャッコ言語はマクロを持たない。
- Rust のライフタイムの記法 `'a` と、文字リテラル `'a'` の構文が字句解析 (特に正規表現ベースの色分け) で衝突しやすい。
    - ジャッコ言語はいまのところライフタイムを持たないので関係ない。

参考:

- [C Needs Better Syntax and Macros – Andrey Orst](https://andreyorst.gitlab.io/posts/2020-04-06-c-needs-a-better-syntax/)

## 構文木・構文解析の実装に関するメモ

ソースコードを字句解析してトークン列を得られたとする。これを構文解析して構文木を得たい。

トリビア (空白やコメントなど) は構文規則的に意味がないのでトークン列から取り除く。

構文解析の目的はトークン列を木構造に変形し、それぞれのノードの種類を特定すること。

### 具象構文木の組み立て

式文の例:

```
入力:
    2 + 3 * 5 + 7;

トークン列:
    2, '+', 3, '*' 5, '+' 7, ';', EOF

構文木:
    [root
        [expr_stmt
            [binary
                [binary
                    [lit 2]
                    +
                    [binary
                        [lit 3]
                        *
                        [lit 5]
                    ]]
                +
                [lit 7]]
            ;]]

工程:

    parse_root
        start_element
        parse_stmt
            start_element
            parse_expr
                parse_add
                    parse_mul
                        parse_atom
                            start_element
                            bump 2
                            finish(lit)

                    start_parent
                    bump +

                    parse_mul
                        parse_atom
                            bump 3
                            finish(lit)
                        start_parent
                        bump *
                        parse_atom
                            start_element
                            bump 5
                            finish(lit)
                        finish(binary)

                    start_parent
                    bump +

                    parse_mul
                        parse_atom
                            start_element
                            bump 7
                            finish(lit)
                    finish(binary)
                finish(binary)
            bump ;
            finish(expr_stmt)
        finish(root)
```
