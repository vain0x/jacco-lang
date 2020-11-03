use crate::{
    front::name_resolution::NameReferents,
    parse::{ANameId, ATree},
    scope::scope_system::{FindKind, ImportKind, ScopeSystem},
};
use std::mem::take;

/// 個別の構文要素にスコープを与えるもの。
///
/// 各メソッドは、構文要素のパースの直前・直後に呼ばれる。
pub(crate) struct SyntaxScopes {
    resolver: ScopeSystem,
}

impl SyntaxScopes {
    pub(crate) fn new() -> Self {
        Self {
            resolver: ScopeSystem::new(),
        }
    }

    pub(crate) fn enter_block(&mut self) {
        self.resolver.enter_scope();
    }

    pub(crate) fn leave_block(&mut self) {
        self.resolver.leave_scope();
    }

    pub(crate) fn on_ty_param_decl(&mut self, name: ANameId, ast: &ATree) {
        // FIXME: 型パラメータの名前の重複はエラーにする
        let text = name.of(ast.names()).text();
        self.resolver
            .on_name_def_stacked(name, ImportKind::Ty, text);
    }

    pub(crate) fn on_param_decl(&mut self, name: ANameId, ast: &ATree) {
        // FIXME: パラメータの名前の重複はエラーにする
        let text = name.of(ast.names()).text();
        self.resolver
            .on_name_def_stacked(name, ImportKind::Value, text);
    }

    pub(crate) fn on_field_decl(&mut self, name: ANameId, ast: &ATree) {
        let text = name.of(ast.names()).text();
        self.resolver.on_name_def_nonlocal(name, text);
    }

    pub(crate) fn on_name_ty(&mut self, name: ANameId, ast: &ATree) {
        #[cfg(skip)]
        log::trace!(
            "on_name_ty {}#{}",
            name.of(ast.names()).head(),
            name.to_index()
        );

        self.resolver
            .on_name_use(name, FindKind::Ty, &name.of(ast.names()).head);
    }

    pub(crate) fn on_name_pat(&mut self, name: ANameId, ast: &ATree) {
        let name_data = name.of(ast.names());
        let head = name.of(ast.names()).head();

        // いまのところパス式の末尾以外は型名。
        if name_data.is_qualified() {
            return self.resolver.on_name_use(name, FindKind::Ty, head);
        }

        // FIXME: const/unit-like struct の可能性もある?
        self.resolver
            .on_name_def_stacked(name, ImportKind::Value, head);
    }

    pub(crate) fn on_record_pat(&mut self, name: ANameId, ast: &ATree) {
        self.resolver
            .on_name_use(name, FindKind::Ty, &name.of(ast.names()).head);
    }

    pub(crate) fn on_name_expr(&mut self, name: ANameId, ast: &ATree) {
        #[cfg(skip)]
        log::trace!(
            "on_name_expr {}#{}",
            name.of(ast.names()).head(),
            name.to_index()
        );

        let name_data = name.of(ast.names());
        let head = name.of(ast.names()).head();

        // いまのところパス式の末尾以外は型名。
        let kind = if name_data.is_qualified() {
            FindKind::Ty
        } else {
            FindKind::Value
        };

        self.resolver.on_name_use(name, kind, head);
    }

    pub(crate) fn enter_arm(&mut self) {
        self.resolver.enter_scope();
    }

    pub(crate) fn leave_arm(&mut self) {
        self.resolver.leave_scope();
    }

    fn leave_stacked_value_decl(
        &mut self,
        #[allow(unused)] hint: &str,
        name_opt: Option<ANameId>,
        ast: &ATree,
    ) {
        #[cfg(skip)]
        log::trace!(
            "leave_stacked_value_decl({}) {}",
            hint,
            name_opt.map_or("name".into(), |name| format!(
                "{}#{}",
                name.of(ast.names()).text(),
                name.to_index()
            ))
        );
        if let Some(name) = name_opt {
            self.resolver
                .on_name_def_stacked(name, ImportKind::Value, name.of(ast.names()).text());
        }
    }

    pub(crate) fn leave_let_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.leave_stacked_value_decl("let", name_opt, ast);
    }

    pub(crate) fn leave_const_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.leave_stacked_value_decl("const", name_opt, ast);
    }

    pub(crate) fn leave_static_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.leave_stacked_value_decl("static", name_opt, ast);
    }

    fn leave_hoisted_decl(
        &mut self,
        #[allow(unused)] hint: &str,
        name_opt: Option<ANameId>,
        kind: ImportKind,
        ast: &ATree,
    ) {
        #[cfg(skip)]
        log::trace!(
            "leave_hoisted_decl({}) {}",
            hint,
            name_opt.map_or("name".into(), |name| format!(
                "{}#{}",
                name.of(ast.names()).text(),
                name.to_index()
            ))
        );
        if let Some(name) = name_opt {
            self.resolver
                .on_name_def_hoisted(name, kind, &name.of(ast.names()).text);
        }
    }

    pub(crate) fn enter_fn_stmt(&mut self) {
        self.resolver.enter_scope();
    }

    pub(crate) fn leave_fn_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.resolver.leave_scope();
        self.leave_hoisted_decl("fn", name_opt, ImportKind::Value, ast);
    }

    pub(crate) fn enter_extern_fn_stmt(&mut self) {
        self.resolver.enter_scope();
    }

    pub(crate) fn leave_extern_fn_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.resolver.leave_scope();
        self.leave_hoisted_decl("extern fn", name_opt, ImportKind::Value, ast);
    }

    pub(crate) fn enter_enum_stmt(&mut self) {
        self.resolver.enter_scope();
    }

    pub(crate) fn leave_enum_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.resolver.leave_scope();
        self.leave_hoisted_decl("enum", name_opt, ImportKind::Ty, ast);
    }

    pub(crate) fn enter_struct_stmt(&mut self) {
        self.resolver.enter_scope();
    }

    pub(crate) fn leave_struct_stmt(
        &mut self,
        name_opt: Option<ANameId>,
        is_unit_like: bool,
        ast: &ATree,
    ) {
        self.resolver.leave_scope();

        // unit-like 構造体は値としても参照できる。
        let kind = if is_unit_like {
            ImportKind::Both
        } else {
            ImportKind::Ty
        };

        self.leave_hoisted_decl("struct", name_opt, kind, ast);
    }

    pub(crate) fn on_use_stmt(&mut self, name_opt: Option<ANameId>, ast: &ATree) {
        self.leave_hoisted_decl("use", name_opt, ImportKind::Both, ast);
    }

    pub(crate) fn finish(&mut self, #[allow(unused)] ast: &ATree) -> NameReferents {
        // 解決結果をログ出力する。
        let mut referents = self
            .resolver
            .name_referents
            .iter()
            .map(|(&name, &referent)| (name, referent))
            .collect::<Vec<_>>();
        referents.sort_by_key(|&(key, _)| key);

        #[cfg(skip)]
        for (name, referent) in referents {
            log::trace!(
                "name#{} ({:?}) -> {:?}",
                name.to_index(),
                ast.names().get(name).map(|name| name.text()),
                referent
            );
        }

        take(&mut self.resolver.name_referents)
    }
}
