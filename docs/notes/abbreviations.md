# 略語表など

実装内で使われている用語など。

- outline は関数の引数の型などの外部に公開されるインターフェイスを指すために使っている。(一般的な用法ではない)
- continuation passing style (CPS): 継続渡し形式
- Gx, Tx など
    - 何らかの処理で使うデータを入れた構造体。なんとか context の略。

| 略語   | 原語           | 意味 |
|:------|:--------------|:----|
| alt   | alternative   | 代替 (else の式) |
| arg   | argument      | 実引数 |
| attr  | attribute     | 属性 |
| cl    | closure       | クロージャ |
| cond  | condition     | 条件 |
| cont  | continuation  | 継続 |
| decl  | declaration   | 宣言 |
| deref | dereference   | 脱参照 (参照を辿る操作) |
| div   | division      | 除算 |
| env   | environment   | 環境 |
| expr  | expression    | 式 |
| fn    | function      | 関数 |
| gen   | generation    | 生成 |
| ident | identifier    | 識別子 |
| init  | initialization| 初期化 |
| lit   | literal       | リテラル |
| mod   | module        | モジュール |
| mul   | multiplication| 乗算 |
| mut   | mutable       | 可変 |
| op    | operation     | 演算 |
| op    | operator      | 演算子 |
| opt   | optional      | 省略可能 |
| param | parameter     | 仮引数 |
| prim  | primitive     | プリミティブ |
| qual  | qualifier     | 修飾子 |
| ref   | reference     | 参照 |
| semi  | semicolon     | セミコロン |
| sig   | signature     | シグネチャ |
| stmt  | statement     | 文 |
| sub   | subtraction   | 減算 |
| ty    | type          | (静的) 型 |

(info などの一般的な略語や、fn などの Rust のキーワードは除く。)
