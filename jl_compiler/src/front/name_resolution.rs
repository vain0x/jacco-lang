//! 名前解決の処理

use crate::{cps::*, front::*, utils::MapStack};
use std::collections::HashMap;

pub(crate) trait NameResolutionListener {
    fn ty_did_resolve(&mut self, loc: PLoc, ty: &KTy);
}

pub(crate) struct NullNameResolutionListener;

impl NameResolutionListener for NullNameResolutionListener {
    fn ty_did_resolve(&mut self, _loc: PLoc, _ty: &KTy) {}
}

pub(crate) struct PathResolutionContext<'a> {
    pub(super) tokens: &'a PTokens,
    pub(super) ast: &'a ATree,
    pub(super) name_referents: &'a NameReferents,
    pub(super) name_symbols: &'a NameSymbols,
    pub(super) k_mod: KMod,
    pub(super) mod_outline: &'a KModOutline,
    pub(super) mod_outlines: &'a KModOutlines,
    pub(super) listener: &'a mut dyn NameResolutionListener,
}

// -----------------------------------------------
// 型
// -----------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum BuiltInTy {
    Unknown,
    Never,
    Number(KNumberTy),
}

impl BuiltInTy {
    pub(crate) fn to_ty(self) -> KTy {
        match self {
            BuiltInTy::Unknown => KTy::Unknown,
            BuiltInTy::Never => KTy::Never,
            BuiltInTy::Number(ty) => KTy::Number(ty),
        }
    }
}

fn resolve_builtin_ty(text: &str) -> Option<BuiltInTy> {
    KNumberTy::parse(text)
        .map(BuiltInTy::Number)
        .or_else(|| match text {
            "unknown" => Some(BuiltInTy::Unknown),
            "never" => Some(BuiltInTy::Never),
            _ => None,
        })
}

pub(crate) fn resolve_ty_name(
    name: ANameId,
    key: ANameKey,
    name_referents: &NameReferents,
    name_symbols: &NameSymbols,
    listener: &mut dyn NameResolutionListener,
) -> Option<KTy> {
    let ty_opt = name_referents.get(&name).and_then(|referent| {
        let ty = match referent {
            BaseReferent::BeforeProcess | BaseReferent::Deferred | BaseReferent::Unresolved => {
                return None;
            }
            BaseReferent::Def => name_symbols.get(&name)?.as_ty()?,
            BaseReferent::Name(def_name) => name_symbols.get(&def_name)?.as_ty()?,
            BaseReferent::BuiltInTy(ty) => ty.to_ty(),
        };
        Some(ty)
    });

    if let Some(ty) = &ty_opt {
        listener.ty_did_resolve(PLoc::Name(key), ty);
    }
    ty_opt
}

fn find_const_variant(
    const_enum: KConstEnum,
    name: &str,
    mod_outline: &KModOutline,
) -> Option<KConst> {
    const_enum
        .variants(&mod_outline.const_enums)
        .iter()
        .find(|variant| variant.of(&mod_outline.consts).name == name)
}

fn find_struct_variant(
    struct_enum: KStructEnum,
    name: &str,
    mod_outline: &KModOutline,
) -> Option<KStruct> {
    struct_enum
        .variants(&mod_outline.struct_enums)
        .iter()
        .copied()
        .find(|k_struct| k_struct.name(&mod_outline.structs) == name)
}

pub(crate) fn resolve_ty_path(
    name: ANameId,
    key: ANameKey,
    context: PathResolutionContext<'_>,
) -> Option<KTy2> {
    let PathResolutionContext {
        tokens,
        ast,
        name_referents,
        name_symbols,
        k_mod,
        mod_outline,
        mod_outlines,
        listener,
    } = context;

    let (_, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            return resolve_ty_name(name, key, name_referents, name_symbols, listener)
                .map(|ty| ty.to_ty2_poly(k_mod, mod_outlines));
        }
    };

    if !tail.is_empty() {
        log::error!("型パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let ty = match resolve_ty_name(name, key, name_referents, name_symbols, listener)? {
        KTy::Alias(alias) => match alias.of(&mod_outline.aliases).referent_as_ty() {
            Some(KTy2::StructEnum(KProjectStructEnum(k_mod, struct_enum))) => {
                let name = name.of(ast.names()).token.text(tokens);
                let k_struct = find_struct_variant(struct_enum, name, k_mod.of(&mod_outlines))?;

                KTy2::Struct(KProjectStruct(k_mod, k_struct))
            }
            _ => return None,
        },
        KTy::StructEnum(struct_enum) => {
            let name = name.of(ast.names()).token.text(tokens);
            let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

            KTy2::Struct(KProjectStruct(k_mod, k_struct))
        }
        _ => return None,
    };
    Some(ty)
}

fn resolve_value_name(
    name: ANameId,
    name_referents: &NameReferents,
    name_symbols: &NameSymbols,
    mod_outline: &KModOutline,
) -> Option<KLocalValue> {
    name_referents
        .get(&name)
        .and_then(|referent| match referent {
            BaseReferent::BeforeProcess
            | BaseReferent::Deferred
            | BaseReferent::Unresolved
            | BaseReferent::BuiltInTy(_) => None,
            BaseReferent::Def => match name_symbols.get(&name)? {
                NameSymbol::TyParam(_) => None,
                NameSymbol::LocalVar(local_var) => Some(KLocalValue::LocalVar(*local_var)),
                NameSymbol::ModSymbol(symbol) => symbol.as_value(mod_outline),
            },
            BaseReferent::Name(def_name) => match name_symbols.get(&def_name)? {
                NameSymbol::TyParam(_) => None,
                NameSymbol::LocalVar(local_var) => Some(KLocalValue::LocalVar(*local_var)),
                NameSymbol::ModSymbol(symbol) => symbol.as_value(mod_outline),
            },
        })
}

pub(crate) fn resolve_value_path(
    name: ANameId,
    key: ANameKey,
    context: PathResolutionContext<'_>,
) -> Option<KProjectValue> {
    let PathResolutionContext {
        tokens,
        ast,
        name_referents,
        name_symbols,
        k_mod,
        mod_outline,
        mod_outlines,
        listener,
    } = context;

    let (_, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            let value = resolve_value_name(name, name_referents, name_symbols, mod_outline)?;
            return Some(KProjectValue::new(k_mod, value));
        }
    };

    if !tail.is_empty() {
        log::error!("パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let value = match resolve_ty_name(name, key, name_referents, name_symbols, listener)? {
        KTy::Alias(alias) => {
            let name = name.of(ast.names()).token.text(tokens);
            let value = match alias.of(&mod_outline.aliases).referent()? {
                KProjectSymbol::ConstEnum(KProjectConstEnum(k_mod, const_enum)) => {
                    let mod_outline = k_mod.of(mod_outlines);
                    let k_const = find_const_variant(const_enum, name, mod_outline)?;
                    KProjectValue::new(k_mod, KLocalValue::Const(k_const))
                }
                KProjectSymbol::StructEnum(KProjectStructEnum(k_mod, struct_enum)) => {
                    let mod_outline = k_mod.of(mod_outlines);
                    let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

                    if k_struct.of(&mod_outline.structs).is_unit_like() {
                        KProjectValue::new(k_mod, KLocalValue::UnitLikeStruct(k_struct))
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };
            return Some(value);
        }
        KTy::ConstEnum(const_enum) => {
            let name = name.of(ast.names()).token.text(tokens);
            let k_const = find_const_variant(const_enum, name, mod_outline)?;
            KLocalValue::Const(k_const)
        }
        KTy::StructEnum(struct_enum) => {
            let name = name.of(ast.names()).token.text(tokens);
            let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

            if k_struct.of(&mod_outline.structs).is_unit_like() {
                KLocalValue::UnitLikeStruct(k_struct)
            } else {
                return None;
            }
        }
        _ => return None,
    };
    Some(KProjectValue::new(k_mod, value))
}

// =============================================================================
// V3
// =============================================================================

/// ルートスコープは 0
type ScopeId = usize;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum BaseReferent {
    #[allow(unused)]
    BeforeProcess,
    Deferred,
    #[allow(unused)]
    Unresolved,
    Def,
    Name(ANameId),
    BuiltInTy(BuiltInTy),
}

pub(crate) type NameReferents = HashMap<ANameId, BaseReferent>;

pub(crate) type NameSymbols = HashMap<ANameId, NameSymbol>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum NameSymbol {
    TyParam(KTyParam),
    LocalVar(KLocalVar),
    ModSymbol(KModSymbol),
}

impl NameSymbol {
    pub(crate) fn as_ty(&self) -> Option<KTy> {
        match self {
            NameSymbol::TyParam(ty_param) => Some(KTy::Var(KTyVar {
                name: ty_param.name.to_string(),
                loc: ty_param.loc,
            })),
            NameSymbol::LocalVar(_) => None,
            NameSymbol::ModSymbol(symbol) => symbol.as_ty(),
        }
    }
}

#[derive(Default)]
pub(crate) struct NameResolver {
    scope_id: ScopeId,
    scope_parents: Vec<ScopeId>,
    value_env: MapStack<ANameId>,
    ty_env: MapStack<ANameId>,
    defer_map: HashMap<(String, FindKind), Vec<(ANameId, ScopeId)>>,
    pub(crate) name_referents: NameReferents,
}

impl<'a> NameResolver {
    pub(crate) fn new() -> Self {
        let mut resolver = Self::default();
        resolver.scope_parents = vec![0];
        resolver.ty_env.push();
        resolver.value_env.push();
        resolver
    }
}

#[derive(Copy, Clone)]
enum ImportKind {
    Value,
    Ty,
    Both,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum FindKind {
    Value,
    Ty,
}

mod v3_core {
    use std::mem::replace;

    use super::*;

    fn scope_breads_list(resolver: &NameResolver) -> String {
        let mut ancestors = vec![];
        let mut scope_id = resolver.scope_id;
        while scope_id != 0 {
            ancestors.push(scope_id.to_string());
            scope_id = resolver.scope_parents[scope_id];
        }
        ancestors.push("0".into());
        ancestors.reverse();
        ancestors.join(" > ")
    }

    fn is_descendant(mut scope_id: ScopeId, resolver: &NameResolver) -> bool {
        loop {
            if scope_id == resolver.scope_id {
                return true;
            }

            if scope_id == 0 {
                return false;
            }

            scope_id = resolver.scope_parents[scope_id];
        }
    }

    pub(super) fn enter_scope(resolver: &mut NameResolver) {
        let parent = replace(&mut resolver.scope_id, resolver.scope_parents.len());
        resolver.scope_parents.push(parent);

        resolver.ty_env.push();
        resolver.value_env.push();
        log::trace!(
            "enter_scope #{} {:?}",
            resolver.scope_id,
            scope_breads_list(resolver)
        );
    }

    pub(super) fn leave_scope(resolver: &mut NameResolver) {
        log::trace!(
            "leave_scope #{} {:?}",
            resolver.scope_id,
            scope_breads_list(resolver)
        );
        resolver.scope_id = resolver.scope_parents[resolver.scope_id];
        resolver.ty_env.pop();
        resolver.value_env.pop();
    }

    fn bind_name(
        hint: &str,
        name_id: ANameId,
        #[allow(unused)] text_opt: Option<&str>,
        referent: BaseReferent,
        expected: Option<BaseReferent>,
        resolver: &mut NameResolver,
    ) {
        log::trace!(
            "bind_name({}) {}#{} ({:?} -> {:?})",
            hint,
            text_opt.unwrap_or(""),
            name_id.to_index(),
            expected,
            referent
        );

        let old = resolver.name_referents.insert(name_id, referent);
        assert_eq!(old, expected);
    }

    fn import_name(name_id: ANameId, kind: ImportKind, text: &str, resolver: &mut NameResolver) {
        match kind {
            ImportKind::Value => {
                log::trace!("import value {} -> {}", text, name_id.to_index());
                resolver.value_env.insert(text.to_string(), name_id);
            }
            ImportKind::Ty => {
                log::trace!("import ty {} -> {}", text, name_id.to_index());
                resolver.ty_env.insert(text.to_string(), name_id);
            }
            ImportKind::Both => {
                log::trace!("import ty/value {} -> {}", text, name_id.to_index());
                resolver.value_env.insert(text.to_string(), name_id);
                resolver.ty_env.insert(text.to_string(), name_id);
            }
        }
    }

    fn find_name(kind: FindKind, text: &str, resolver: &mut NameResolver) -> Option<BaseReferent> {
        match kind {
            FindKind::Value => resolver
                .value_env
                .get(text)
                .map(|&name| BaseReferent::Name(name)),
            FindKind::Ty => resolver
                .ty_env
                .get(text)
                .map(|&name| BaseReferent::Name(name))
                .or_else(|| resolve_builtin_ty(text).map(|ty| BaseReferent::BuiltInTy(ty))),
        }
    }

    /// 前方参照を許さない名前が追加される。
    pub(super) fn on_name_def_stacked(
        name: ANameId,
        kind: ImportKind,
        text: &str,
        resolver: &mut NameResolver,
    ) {
        import_name(name, kind, text, resolver);
        bind_name("stack", name, Some(text), BaseReferent::Def, None, resolver)
    }

    /// 前方参照を許す値の名前を追加する。
    pub(super) fn on_name_def_hoisted(
        name: ANameId,
        kind: ImportKind,
        text: &str,
        resolver: &mut NameResolver,
    ) {
        let referent = BaseReferent::Name(name);

        let mut aux = |kind: FindKind| {
            // この定義がみえる位置にある前方参照を解決する。
            let key = (text.to_string(), kind);
            let mut vec = match resolver.defer_map.remove(&key) {
                Some(it) => it,
                None => return,
            };

            vec.retain(|&(name, pos)| {
                let can_see = is_descendant(pos, resolver);
                if !can_see {
                    // pos からこの定義が見えない。(`{ f(); } { fn f() {} }` のようなケース)
                    return true;
                }

                let expected = Some(BaseReferent::Deferred);
                bind_name("hoist", name, None, referent, expected, resolver);
                false
            });

            if !vec.is_empty() {
                resolver.defer_map.insert(key, vec);
            }
        };

        match kind {
            ImportKind::Value => aux(FindKind::Value),
            ImportKind::Ty => aux(FindKind::Ty),
            ImportKind::Both => {
                aux(FindKind::Value);
                aux(FindKind::Ty);
            }
        }

        import_name(name, kind, text, resolver);
        bind_name("hoist", name, Some(text), BaseReferent::Def, None, resolver);
    }

    pub(super) fn on_name_use(
        name: ANameId,
        kind: FindKind,
        text: &str,
        resolver: &mut NameResolver,
    ) {
        let referent = match find_name(kind, text, resolver) {
            Some(it) => it,
            None => {
                resolver
                    .defer_map
                    .entry((text.to_string(), kind))
                    .or_insert(vec![])
                    .push((name, resolver.scope_id));
                BaseReferent::Deferred
            }
        };

        bind_name("use", name, Some(text), referent, None, resolver);
    }
}

pub(crate) mod v3 {
    use super::*;

    pub(crate) fn enter_block(resolver: &mut NameResolver) {
        v3_core::enter_scope(resolver);
    }

    pub(crate) fn leave_block(resolver: &mut NameResolver) {
        v3_core::leave_scope(resolver);
    }

    pub(crate) fn on_ty_param_decl(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        // FIXME: 型パラメータの名前の重複はエラーにする
        let text = name.of(ast.names()).text();
        v3_core::on_name_def_stacked(name, ImportKind::Ty, text, resolver);
    }

    pub(crate) fn on_param_decl(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        // FIXME: パラメータの名前の重複はエラーにする
        let text = name.of(ast.names()).text();
        v3_core::on_name_def_stacked(name, ImportKind::Value, text, resolver);
    }

    pub(crate) fn on_name_ty(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        log::trace!(
            "on_name_ty {}#{}",
            name.of(ast.names()).head(),
            name.to_index()
        );

        v3_core::on_name_use(name, FindKind::Ty, name.of(ast.names()).head(), resolver);
    }

    pub(crate) fn on_name_pat(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        let name_data = name.of(ast.names());
        let head = name.of(ast.names()).head();

        // いまのところパス式の末尾以外は型名。
        if name_data.is_qualified() {
            return v3_core::on_name_use(name, FindKind::Ty, head, resolver);
        }

        // FIXME: const/unit-like struct の可能性もある?
        v3_core::on_name_def_stacked(name, ImportKind::Value, head, resolver);
    }

    pub(crate) fn on_record_pat(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        v3_core::on_name_use(name, FindKind::Ty, name.of(ast.names()).head(), resolver);
    }

    pub(crate) fn on_name_expr(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
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

        v3_core::on_name_use(name, kind, head, resolver);
    }

    pub(crate) fn enter_arm(resolver: &mut NameResolver) {
        v3_core::enter_scope(resolver);
    }

    pub(crate) fn leave_arm(resolver: &mut NameResolver) {
        v3_core::leave_scope(resolver);
    }

    fn leave_stacked_value_decl(
        hint: &str,
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
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
            v3_core::on_name_def_stacked(
                name,
                ImportKind::Value,
                name.of(ast.names()).text(),
                resolver,
            );
        }
    }

    pub(crate) fn leave_let_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        leave_stacked_value_decl("let", name_opt, ast, resolver);
    }

    pub(crate) fn leave_const_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        leave_stacked_value_decl("const", name_opt, ast, resolver);
    }

    pub(crate) fn leave_static_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        leave_stacked_value_decl("static", name_opt, ast, resolver);
    }

    fn leave_hoisted_decl(
        hint: &str,
        name_opt: Option<ANameId>,
        kind: ImportKind,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
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
            v3_core::on_name_def_hoisted(name, kind, name.of(ast.names()).text(), resolver);
        }
    }

    pub(crate) fn enter_fn_decl(resolver: &mut NameResolver) {
        v3_core::enter_scope(resolver);
    }

    pub(crate) fn leave_fn_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        v3_core::leave_scope(resolver);
        leave_hoisted_decl("fn", name_opt, ImportKind::Value, ast, resolver);
    }

    pub(crate) fn enter_extern_fn_decl(resolver: &mut NameResolver) {
        v3_core::enter_scope(resolver);
    }

    pub(crate) fn leave_extern_fn_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        v3_core::leave_scope(resolver);
        leave_hoisted_decl("extern fn", name_opt, ImportKind::Value, ast, resolver);
    }

    pub(crate) fn enter_enum_decl(resolver: &mut NameResolver) {
        v3_core::enter_scope(resolver);
    }

    pub(crate) fn leave_enum_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        v3_core::leave_scope(resolver);
        leave_hoisted_decl("enum", name_opt, ImportKind::Ty, ast, resolver);
    }

    pub(crate) fn enter_struct_decl(resolver: &mut NameResolver) {
        v3_core::enter_scope(resolver);
    }

    pub(crate) fn leave_struct_decl(
        name_opt: Option<ANameId>,
        is_unit_like: bool,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        v3_core::leave_scope(resolver);

        // unit-like 構造体は値としても参照できる。
        let kind = if is_unit_like {
            ImportKind::Both
        } else {
            ImportKind::Ty
        };

        leave_hoisted_decl("struct", name_opt, kind, ast, resolver);
    }

    pub(crate) fn on_use_decl(name_opt: Option<ANameId>, ast: &ATree, resolver: &mut NameResolver) {
        leave_hoisted_decl("use", name_opt, ImportKind::Both, ast, resolver);
    }

    pub(crate) fn finish(resolver: &NameResolver, ast: &ATree) {
        let mut referents = resolver
            .name_referents
            .iter()
            .map(|(&name, &referent)| (name, referent))
            .collect::<Vec<_>>();
        referents.sort_by_key(|&(key, _)| key);

        for (name, referent) in referents {
            log::trace!(
                "name#{} ({:?}) -> {:?}",
                name.to_index(),
                ast.names().get(name).map(|name| name.text()),
                referent
            );
        }
    }
}
