use crate::{
    parse::*,
    utils::{DebugWith, DebugWithContext},
};
use std::fmt::{self, Debug, Formatter};

#[derive(Clone)]
pub(crate) struct PRoot {
    pub(crate) eof: PToken,
    pub(crate) elements: PElementArena,
    pub(crate) skipped: Vec<PToken>,
    pub(crate) tokens: PTokens,
    pub(crate) ast: ATree,
    pub(crate) root: PElement,
}

impl PRoot {
    pub(crate) fn write_trace(&self) {
        log::trace!(
            "SyntaxTree (untyped):\n{:#?}",
            DebugWith::new(&self.root, self)
        );

        dump_ast(self);
    }
}

impl DebugWithContext<PRoot> for PToken {
    fn fmt(&self, root: &PRoot, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.of(&root.tokens), f)
    }
}

impl DebugWithContext<PRoot> for PElement {
    fn fmt(&self, root: &PRoot, f: &mut Formatter<'_>) -> fmt::Result {
        let data = self.of(&root.elements);
        write!(f, "{:?} ", data.kind())?;

        f.debug_list()
            .entries(
                data.children()
                    .iter()
                    .map(|child| DebugWith::new(child, root)),
            )
            .finish()
    }
}

impl DebugWithContext<PRoot> for PNode {
    fn fmt(&self, context: &PRoot, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PNode::Token(inner) => DebugWithContext::fmt(inner, context, f),
            PNode::Element(inner) => DebugWithContext::fmt(inner, context, f),
        }
    }
}

impl Debug for PRoot {
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

impl PRoot {
    /// use 宣言で指名されているモジュールの名前を収集する。(いまのところトップレベルにある use 宣言の先頭にある名前だけを収集する。)
    pub(crate) fn collect_used_mod_names(&self, mod_names: &mut Vec<String>) {
        for decl in self.ast.decls.iter() {
            match decl {
                ADecl::Use(AUseDecl {
                    name_opt: Some(name),
                    ..
                }) => mod_names.push(name.root_text().to_string()),
                _ => {}
            }
        }
    }
}
