use crate::{
    parse::*,
    utils::{DebugWith, DebugWithContext},
};
use std::fmt::{self, Debug, Formatter};

/// 構文木
pub(crate) struct PTree {
    /// EOF トークン
    /// パースが末尾に到達したことを保証するためのもの。
    #[allow(unused)]
    pub(crate) eof: PToken,
    /// 型なし具象構文木のルート要素
    pub(crate) root: PElement,
    /// トークン列。
    /// トリビアは除く。
    pub(crate) tokens: PTokens,
    /// パーサーがエラー回復のためにスキップしたトークンのリスト
    pub(crate) skipped: Vec<PToken>,
    /// 型なし具象構文木の要素のデータ
    pub(crate) elements: PElementArena,
    /// 型つき抽象構文木
    pub(crate) ast: ATree,
}

impl PTree {
    pub(crate) fn write_trace(&self) {
        log::trace!(
            "SyntaxTree (untyped):\n{:#?}",
            DebugWith::new(&self.root, self)
        );

        dump_ast(self);
    }
}

impl DebugWithContext<PTree> for PToken {
    fn fmt(&self, tree: &PTree, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.of(&tree.tokens), f)
    }
}

impl DebugWithContext<PTree> for PElement {
    fn fmt(&self, tree: &PTree, f: &mut Formatter<'_>) -> fmt::Result {
        let data = self.of(&tree.elements);
        write!(f, "{:?} ", data.kind())?;

        f.debug_list()
            .entries(
                data.children()
                    .iter()
                    .map(|child| DebugWith::new(child, tree)),
            )
            .finish()
    }
}

impl DebugWithContext<PTree> for PNode {
    fn fmt(&self, tree: &PTree, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PNode::Token(inner) => DebugWithContext::fmt(inner, tree, f),
            PNode::Element(inner) => DebugWithContext::fmt(inner, tree, f),
        }
    }
}

impl Debug for PTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        DebugWithContext::fmt(&self.root, self, f)?;

        if !self.skipped.is_empty() {
            write!(f, "\n\nskipped:")?;
            f.debug_list()
                .entries(self.skipped.iter().map(|token| token.of(&self.tokens)))
                .finish()?;
        }

        write!(f, "\n")
    }
}

impl PTree {
    /// use 宣言で指名されているモジュールの名前を収集する。(いまのところトップレベルにある use 宣言の先頭にある名前だけを収集する。)
    pub(crate) fn collect_used_mod_names(&self, mod_names: &mut Vec<String>) {
        for decl in self.ast.decls.iter() {
            match decl {
                ADecl::Use(AUseDecl {
                    name_opt: Some(name),
                    ..
                }) => mod_names.push(name.root_text(&self.tokens).to_string()),
                _ => {}
            }
        }
    }
}
