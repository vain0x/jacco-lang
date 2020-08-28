# 字句解析

文字列として与えられたソースコードをトークンの列にする。トークンはソースコード上に出現する「分割不能な単位」であり、主に以下の種類がある。

- EOF: 入力の終端
    - 必須ではないが、構文解析時の番兵として便利
- トリビア: 構文的に意味を持たない部分
    - shebang (`#!/bin/sh`)
    - 空白 (\x20, \t)
    - 改行 (\n, \r\n)
    - コメント (`// ...` や `/* ... */`)
    - 解釈不能 (ヌル文字、ソースコードの文字列エンコーディングに違反している部分、ソースコードの地の文に出現しえない文字など)
        - BOM
- リテラル:
    - 数値リテラル: 42 とか `0.314e+1` とか
    - 文字リテラル: `'a'` など
    - 文字列リテラル: `"hello"` など
- 識別子:
    - キーワードや予約語である識別子
        - `fn` など
    - キーワードでない識別子
        - `f` など
- 記号
    - `+` や `+=` など

## トークン列のイメージ

字句解析の目的は文字列をいくつかの区間に分割し、それぞれの区間のトークンとしての種類を特定すること。

例:

```
    fn main() {}

^^^^ 空白
    ^^ キーワード(fn)
      ^ 空白
```

```
(続き)
    fn main() {}
       ^^^^ 識別子
           ^ 左丸カッコ
            ^ 右丸カッコ
             ^ 空白
```

```
(続き)
    fn main() {}
              ^ 左波カッコ
               ^ 右波カッコ
                | EOF
```