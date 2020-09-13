use crate::utils::{VecArena, VecArenaId};

pub(crate) struct ScopeTag;

pub(crate) type ScopeId = VecArenaId<ScopeTag>;

struct ScopeData {
    /// スコープの名前 (デバッグ用)
    hint: String,

    /// 親スコープ。ルートのみ None。
    parent_opt: Option<ScopeId>,
}

/// スコープからなる木構造を DFS 順序で訪問しながら、スコープの親子関係を記録していくもの。
pub(crate) struct ScopeWalker {
    current: ScopeId,
    scopes: VecArena<ScopeTag, ScopeData>,
}

impl ScopeWalker {
    pub(crate) fn new() -> Self {
        let mut scopes = VecArena::new();
        let root = scopes.alloc(ScopeData {
            hint: "".to_string(),
            parent_opt: None,
        });

        Self {
            current: root,
            scopes,
        }
    }

    pub(crate) fn current(&self) -> ScopeId {
        self.current
    }

    /// デバッグ用: いまのスコープまでの祖先を列挙する。
    pub(crate) fn breadcrumbs(&self) -> String {
        let mut ancestors = vec![];
        let mut scope_id = self.current;

        while let Some(parent) = {
            ancestors.push(format!(
                "{}#{}",
                scope_id.of(&self.scopes).hint.as_str(),
                scope_id.to_index()
            ));
            scope_id.of(&self.scopes).parent_opt
        } {
            scope_id = parent;
        }

        ancestors.reverse();
        ancestors.join(" > ")
    }

    pub(crate) fn is_descendant(&self, mut descendant: ScopeId) -> bool {
        while let Some(parent) = {
            if descendant == self.current {
                return true;
            }
            descendant.of(&self.scopes).parent_opt
        } {
            descendant = parent;
        }

        false
    }

    /// 子スコープに入る。
    pub(crate) fn enter(&mut self, hint: impl Into<String>) {
        let child = self.scopes.alloc(ScopeData {
            hint: hint.into(),
            parent_opt: Some(self.current),
        });
        self.current = child;
    }

    /// 親スコープに戻る。
    pub(crate) fn leave(&mut self) {
        self.current = self
            .current
            .of(&self.scopes)
            .parent_opt
            .expect("leave from root scope never");
    }
}
