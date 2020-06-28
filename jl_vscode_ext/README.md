# ジャッコ言語 VSCode 拡張機能

ジャッコ言語の文法の定義や、入力支援機能を提供する拡張機能です。

## インストール

- Visual Studio Code で次の拡張機能をインストールしてください:
    - `vain0x.jacco-lang`

その後、ジャッコ言語のファイル (.jacco) を開くと動作するはずです。

## 機能

- [x] シンタックスハイライト
- [ ] ホバー表示
- [ ] ドキュメントハイライト
- [ ] 定義や参照への移動、一覧
- [ ] 名前の変更
- [ ] 入力補完

----

## 開発環境

この拡張機能の開発環境を構築する手順を説明します。

インストールするもの:

- Visual Studio Code
- Node.js (>= 12)
- Git
    - Windows では Git Bash でシェルスクリプトを実行することを推奨

ビルドスクリプトなど:

- `install`: 拡張機能をソースコードからビルドして、VSCode にインストールします。
    - 注意: バージョン番号が同じものをインストールすると、内容が違っていても更新されないことがあります。
- `uninstall`: VSCode からジャッコ言語の拡張機能をアンインストールします。

## 参考

- Rust の構文の定義: [vscode/rust.tmLanguage.json at 96410ff870b01dbe043cbc5f025af3ec19b8ca2e · microsoft/vscode](https://github.com/microsoft/vscode/blob/96410ff870b01dbe043cbc5f025af3ec19b8ca2e/extensions/rust/syntaxes/rust.tmLanguage.json)
- LSP の仕様書: [Overview](https://microsoft.github.io//language-server-protocol/overviews/lsp/overview/)
    - 有志による和訳: [tennashi/lsp_spec_ja\: LSP 仕様の日本語訳](https://github.com/tennashi/lsp_spec_ja)
- [LSP学習記 #1](https://qiita.com/vain0x/items/d050fe7c8b342ed2004e)
