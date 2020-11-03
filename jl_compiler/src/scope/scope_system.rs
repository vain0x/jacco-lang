use super::{
    lexical_referent::LexicalReferent,
    scope_walker::{ScopeId, ScopeWalker},
};
use crate::{
    front::name_resolution::{resolve_builtin_ty, NameReferents},
    parse::ANameId,
    utils::MapStack,
};
use std::collections::HashMap;

/// スコープの種類 (名前を探すとき)
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum FindKind {
    Ty,
    Value,
}

/// スコープの種類 (名前を導入するとき)
#[derive(Copy, Clone)]
pub(crate) enum ImportKind {
    Ty,
    Value,
    Both,
}

/// スコープ解決の枠組みを提供するもの。
pub(crate) struct ScopeSystem {
    scope_walker: ScopeWalker,
    ty_env: MapStack<ANameId>,
    value_env: MapStack<ANameId>,
    defer_map: HashMap<(String, FindKind), Vec<(ANameId, ScopeId)>>,
    pub(crate) name_referents: NameReferents,
}

impl ScopeSystem {
    pub(crate) fn new() -> Self {
        let mut it = Self {
            scope_walker: ScopeWalker::new(),
            ty_env: MapStack::new(),
            value_env: MapStack::new(),
            defer_map: HashMap::new(),
            name_referents: NameReferents::new(),
        };

        it.ty_env.push();
        it.value_env.push();
        it
    }

    pub(crate) fn enter_scope(&mut self) {
        // FIXME: hint をつける
        self.scope_walker.enter("");
        self.ty_env.push();
        self.value_env.push();

        #[cfg(skip)]
        log::trace!("enter_scope {}", self.scope_walker.breadcrumbs());
    }

    pub(crate) fn leave_scope(&mut self) {
        #[cfg(skip)]
        log::trace!("leave_scope {}", self.scope_walker.breadcrumbs());

        self.scope_walker.leave();
        self.ty_env.pop();
        self.value_env.pop();
    }

    fn bind_name(
        &mut self,
        #[allow(unused)] hint: &str,
        name_id: ANameId,
        #[allow(unused)] text_opt: Option<&str>,
        referent: LexicalReferent,
        expected: Option<LexicalReferent>,
    ) {
        #[cfg(skip)]
        log::trace!(
            "bind_name({}) {}#{} ({:?} -> {:?})",
            hint,
            text_opt.unwrap_or(""),
            name_id.to_index(),
            expected,
            referent
        );

        let old = self.name_referents.insert(name_id, referent);
        assert_eq!(old, expected);
    }

    fn import_name(&mut self, name_id: ANameId, kind: ImportKind, text: &str) {
        match kind {
            ImportKind::Ty => {
                #[cfg(skip)]
                log::trace!("import ty {} -> {}", text, name_id.to_index());

                self.ty_env.insert(text.to_string(), name_id);
            }
            ImportKind::Value => {
                #[cfg(skip)]
                log::trace!("import value {} -> {}", text, name_id.to_index());

                self.value_env.insert(text.to_string(), name_id);
            }
            ImportKind::Both => {
                #[cfg(skip)]
                log::trace!("import ty/value {} -> {}", text, name_id.to_index());

                self.value_env.insert(text.to_string(), name_id);
                self.ty_env.insert(text.to_string(), name_id);
            }
        }
    }

    fn find_name(&mut self, kind: FindKind, text: &str) -> Option<LexicalReferent> {
        match kind {
            FindKind::Ty => self
                .ty_env
                .get(text)
                .map(|&name| LexicalReferent::Name(name))
                .or_else(|| resolve_builtin_ty(text).map(|ty| LexicalReferent::BuiltInTy(ty))),
            FindKind::Value => self
                .value_env
                .get(text)
                .map(|&name| LexicalReferent::Name(name)),
        }
    }

    /// 名前が追加される。ローカルスコープにはインポートされない。
    pub(crate) fn on_name_def_nonlocal(&mut self, name: ANameId, text: &str) {
        self.bind_name("nonlocal", name, Some(text), LexicalReferent::Def, None);
    }

    /// 前方参照を許さない名前が追加される。
    pub(crate) fn on_name_def_stacked(&mut self, name: ANameId, kind: ImportKind, text: &str) {
        self.import_name(name, kind, text);
        self.bind_name("stack", name, Some(text), LexicalReferent::Def, None)
    }

    /// 前方参照を許す値の名前を追加する。
    pub(crate) fn on_name_def_hoisted(&mut self, name: ANameId, kind: ImportKind, text: &str) {
        let referent = LexicalReferent::Name(name);

        let mut aux = |kind: FindKind| {
            // この定義がみえる位置にある前方参照を解決する。
            let key = (text.to_string(), kind);
            let mut vec = match self.defer_map.remove(&key) {
                Some(it) => it,
                None => return,
            };

            vec.retain(|&(name, use_scope)| {
                let can_see = self.scope_walker.is_descendant(use_scope);
                if !can_see {
                    // use_scope からこの定義が見えない。(`{ f(); } { fn f() {} }` のようなケース)
                    return true;
                }

                let expected = Some(LexicalReferent::Unresolved);
                self.bind_name("hoist", name, None, referent, expected);
                false
            });

            if !vec.is_empty() {
                self.defer_map.insert(key, vec);
            }
        };

        match kind {
            ImportKind::Ty => aux(FindKind::Ty),
            ImportKind::Value => aux(FindKind::Value),
            ImportKind::Both => {
                aux(FindKind::Ty);
                aux(FindKind::Value);
            }
        }

        self.import_name(name, kind, text);
        self.bind_name("hoist", name, Some(text), LexicalReferent::Def, None);
    }

    pub(crate) fn on_name_use(&mut self, name: ANameId, kind: FindKind, text: &str) {
        let referent = match self.find_name(kind, text) {
            Some(it) => it,
            None => {
                self.defer_map
                    .entry((text.to_string(), kind))
                    .or_insert(vec![])
                    .push((name, self.scope_walker.current()));
                LexicalReferent::Unresolved
            }
        };

        self.bind_name("use", name, Some(text), referent, None);
    }
}
