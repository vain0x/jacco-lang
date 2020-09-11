//! 名前解決の処理

use crate::{cps::*, front::env::Env, front::*, utils::VecArena};
use env::map_stack::MapStack;
use std::collections::HashMap;

pub(crate) trait NameResolutionListener {
    fn ty_did_resolve(&mut self, loc: PLoc, ty: &KTy);
}

pub(crate) struct NullNameResolutionListener;

impl NameResolutionListener for NullNameResolutionListener {
    fn ty_did_resolve(&mut self, _loc: PLoc, _ty: &KTy) {}
}

pub(crate) type DeclSymbols = VecArena<ADeclTag, Option<KModSymbol>>;

pub(crate) struct PathResolutionContext<'a> {
    pub(super) tokens: &'a PTokens,
    pub(super) ast: &'a ATree,
    pub(super) k_mod: KMod,
    pub(super) mod_outline: &'a KModOutline,
    pub(super) mod_outlines: &'a KModOutlines,
    pub(super) env: &'a Env,
    pub(super) listener: &'a mut dyn NameResolutionListener,
}

pub(crate) fn decl_allows_forward_reference(decl: &ADecl) -> bool {
    match decl {
        ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) | ADecl::Const(_) | ADecl::Static(_) => false,
        ADecl::Fn(_) | ADecl::ExternFn(_) | ADecl::Struct(_) | ADecl::Enum(_) | ADecl::Use(_) => {
            true
        }
    }
}

fn decl_to_name_symbol_pair(
    decl_id: ADeclId,
    decl_symbols: &DeclSymbols,
    mod_outline: &KModOutline,
) -> Option<(String, KModSymbol)> {
    let symbol = decl_symbols.get(decl_id).copied().flatten()?;
    let name = symbol.name(mod_outline);
    Some((name.to_string(), symbol))
}

pub(crate) fn do_add_ty_symbol_to_local_env(name: &str, symbol: KModSymbol, env: &mut Env) {
    let ty = match symbol {
        KModSymbol::Alias(alias) => KTy::Alias(alias),
        KModSymbol::Const(_)
        | KModSymbol::StaticVar(_)
        | KModSymbol::Fn(_)
        | KModSymbol::ExternFn(_)
        | KModSymbol::Field(_) => return,
        KModSymbol::ConstEnum(const_enum) => KTy::ConstEnum(const_enum),
        KModSymbol::StructEnum(struct_enum) => KTy::StructEnum(struct_enum),
        KModSymbol::Struct(k_struct) => KTy::Struct(k_struct),
    };

    env.insert_ty(name.to_string(), ty);
}

fn do_add_value_symbol_to_local_env(
    name: &str,
    symbol: KModSymbol,
    mod_outline: &KModOutline,
    env: &mut Env,
) {
    let value = match symbol {
        KModSymbol::Alias(alias) => KLocalValue::Alias(alias),
        KModSymbol::Const(k_const) => KLocalValue::Const(k_const),
        KModSymbol::StaticVar(static_var) => KLocalValue::StaticVar(static_var),
        KModSymbol::Fn(k_fn) => KLocalValue::Fn(k_fn),
        KModSymbol::ExternFn(extern_fn) => KLocalValue::ExternFn(extern_fn),
        KModSymbol::Struct(k_struct) if k_struct.of(&mod_outline.structs).is_unit_like() => {
            KLocalValue::UnitLikeStruct(k_struct)
        }
        KModSymbol::ConstEnum(_)
        | KModSymbol::StructEnum(_)
        | KModSymbol::Struct(_)
        | KModSymbol::Field(_) => return,
    };

    env.insert_value(name.to_string(), value)
}

fn add_symbol_to_local_env(
    name: &str,
    symbol: KModSymbol,
    mod_outline: &KModOutline,
    env: &mut Env,
) {
    do_add_ty_symbol_to_local_env(name, symbol, env);
    do_add_value_symbol_to_local_env(name, symbol, mod_outline, env);
}

pub(crate) fn add_decl_to_local_env(
    decl_id: ADeclId,
    _decl: &ADecl,
    decl_symbols: &DeclSymbols,
    mod_outline: &KModOutline,
    env: &mut Env,
) {
    // FIXME: let は decl_symbols に登録されていないので、ここでは環境に登録されない。

    if let Some((name, symbol)) = decl_to_name_symbol_pair(decl_id, &decl_symbols, mod_outline) {
        add_symbol_to_local_env(&name, symbol, mod_outline, env);
    }
}

// -----------------------------------------------
// 型
// -----------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BuiltInTy {
    Number(KNumberTy),
    Unknown,
    Never,
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

fn resolve_builtin_ty_name(name: &str) -> Option<KTy> {
    KNumberTy::parse(name)
        .map(KTy::Number)
        .or_else(|| match name {
            "unknown" => Some(KTy::Unknown),
            "never" => Some(KTy::Never),
            _ => None,
        })
}

pub(crate) fn resolve_ty_name(
    name: &str,
    key: ANameKey,
    env: &Env,
    listener: &mut dyn NameResolutionListener,
) -> Option<KTy> {
    let ty_opt = env
        .find_ty(name)
        .cloned()
        .or_else(|| resolve_builtin_ty_name(name));

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
        k_mod,
        mod_outline,
        mod_outlines,
        env,
        listener,
    } = context;

    let (head, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            return resolve_ty_name(name.of(ast.names()).text(), key, env, listener)
                .map(|ty| ty.to_ty2_poly(k_mod, mod_outlines));
        }
    };

    if !tail.is_empty() {
        log::error!("型パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let ty = match resolve_ty_name(head.text(tokens), key, env, listener)? {
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

fn resolve_value_name(name: &str, env: &Env) -> Option<KLocalValue> {
    env.find_value(name)
}

pub(crate) fn resolve_value_path(
    name: ANameId,
    key: ANameKey,
    context: PathResolutionContext<'_>,
) -> Option<KProjectValue> {
    let PathResolutionContext {
        tokens,
        ast,
        k_mod,
        mod_outline,
        mod_outlines,
        env,
        listener,
    } = context;

    let (head, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            let value = resolve_value_name(&name.of(ast.names()).text, env)?;
            return Some(KProjectValue::new(k_mod, value));
        }
    };

    if !tail.is_empty() {
        log::error!("パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let value = match resolve_ty_name(head.text(tokens), key, env, listener)? {
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

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
struct ScopePos {
    /// スコープの深さ (親スコープの個数)
    depth: usize,
    /// スコープの深さごとのインデックス (前方にある同じ深さのスコープの個数)
    index: usize,
}

impl ScopePos {
    /// この位置のスコープから `def_pos` のスコープに含まれる定義が見えるか？ (定義は hoist されるものとする。定義が後方にあるケースにしか使わない。)
    ///
    /// 例 1. `{ f(); fn f() {} }` なら使用 (`f()`) と定義 (`fn f`) が同じスコープにあるので、みえる。
    /// 例 2. `{ { f(); } fn f() {} }` のとき、使用は定義と異なるスコープにあるが、定義の方が浅いスコープにあるので見える。
    /// 例 3. `{ f(); { fn f() {} } }` のとき使用箇所より定義の方が深いスコープにあるので見えない。
    /// 例 4. `{ f(); } { fn f() {} }` のとき使用箇所と定義箇所は同じ深さにあるが、異なるスコープにあるので見えない。
    #[allow(unused)]
    pub(crate) fn can_see(self, def_pos: ScopePos) -> bool {
        // 定義がより浅いブロックにあるか、同一のブロックにあるなら OK.
        def_pos.depth < self.depth || def_pos == self
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum BaseReferent {
    #[allow(unused)]
    BeforeProcess,
    Deferred,
    #[allow(unused)]
    Unresolved,
    Def,
    Name(ANameId),
    BuiltInTy(BuiltInTy),
}

#[derive(Default)]
pub(crate) struct NameResolver {
    pos: ScopePos,
    /// `depth_counts[d]`: 深さ d のスコープの個数
    depth_counts: Vec<usize>,
    value_env: MapStack<ANameId>,
    ty_env: MapStack<ANameId>,
    defer_map: HashMap<(String, FindKind), Vec<ANameId>>,
    name_referents: HashMap<ANameId, BaseReferent>,
}

impl<'a> NameResolver {
    pub(crate) fn new() -> Self {
        let mut resolver = Self::default();
        resolver.depth_counts = vec![0; 8];
        resolver.ty_env.push();
        resolver.value_env.push();
        resolver
    }
}

#[derive(Copy, Clone)]
enum ImportKind {
    Value,
    #[allow(unused)]
    Ty,
    #[allow(unused)]
    Both,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum FindKind {
    Value,
    Ty,
}

mod v3_core {
    use super::*;

    fn inc_pos(resolver: &mut NameResolver) {
        let d = resolver.pos.depth + 1;
        if d >= resolver.depth_counts.len() {
            resolver.depth_counts.push(0);
        }
        resolver.depth_counts[d] += 1;

        resolver.pos.depth = d;
        resolver.pos.index = resolver.depth_counts[d];
    }

    fn dec_pos(resolver: &mut NameResolver) {
        resolver.pos.depth -= 1;
    }

    pub(super) fn enter_scope(resolver: &mut NameResolver) {
        resolver.ty_env.push();
        resolver.value_env.push();
        inc_pos(resolver);
        log::trace!("enter_scope pos={:?}", resolver.pos);
    }

    pub(super) fn leave_scope(resolver: &mut NameResolver) {
        log::trace!("leave_scope pos={:?}", resolver.pos);
        resolver.ty_env.pop();
        resolver.value_env.pop();
        dec_pos(resolver);
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
    // FIXME: ブロックの深さと位置を見る必要がある。例: `{ f(); } { fn f() {} } fn f() {}` や `f(); fn() {} f() {}` など
    pub(super) fn on_name_def_hoisted(
        name: ANameId,
        kind: ImportKind,
        text: &str,
        resolver: &mut NameResolver,
    ) {
        let mut aux = |kind: FindKind| {
            let names = match resolver.defer_map.remove(&(text.to_string(), kind)) {
                Some(it) => it,
                None => return,
            };

            let referent = BaseReferent::Name(name);
            for name in names {
                let expected = Some(BaseReferent::Deferred);
                bind_name("hoist", name, None, referent, expected, resolver);
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
                    .push(name);
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

    pub(crate) fn on_name_expr(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        log::trace!(
            "on_name_expr {}#{}",
            name.of(ast.names()).text(),
            name.to_index()
        );

        v3_core::on_name_use(name, FindKind::Value, name.of(ast.names()).text(), resolver);
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

    fn leave_hoisted_value_decl(
        hint: &str,
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        log::trace!(
            "leave_hoisted_value_decl({}) {}",
            hint,
            name_opt.map_or("name".into(), |name| format!(
                "{}#{}",
                name.of(ast.names()).text(),
                name.to_index()
            ))
        );
        if let Some(name) = name_opt {
            v3_core::on_name_def_hoisted(
                name,
                ImportKind::Value,
                name.of(ast.names()).text(),
                resolver,
            );
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
        leave_hoisted_value_decl("fn", name_opt, ast, resolver);
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
        leave_hoisted_value_decl("extern fn", name_opt, ast, resolver);
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
