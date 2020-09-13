//! 名前解決の処理

use crate::{
    cps::*, front::*, scope::lexical_referent::LexicalReferent, scope::scope_system::ScopeSystem,
};
use std::collections::HashMap;

pub(crate) struct PathResolutionContext<'a> {
    pub(super) tokens: &'a PTokens,
    pub(super) ast: &'a ATree,
    pub(super) name_referents: &'a NameReferents,
    pub(super) name_symbols: &'a NameSymbols,
    pub(super) k_mod: KMod,
    pub(super) mod_outline: &'a KModOutline,
    pub(super) mod_outlines: &'a KModOutlines,
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

pub(crate) fn resolve_builtin_ty(text: &str) -> Option<BuiltInTy> {
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
    name_referents: &NameReferents,
    name_symbols: &NameSymbols,
) -> Option<KTy> {
    name_referents.get(&name).and_then(|referent| {
        let ty = match referent {
            LexicalReferent::Unresolved => {
                return None;
            }
            LexicalReferent::Def => name_symbols.get(&name)?.as_ty()?,
            LexicalReferent::Name(def_name) => name_symbols.get(&def_name)?.as_ty()?,
            LexicalReferent::BuiltInTy(ty) => ty.to_ty(),
        };
        Some(ty)
    })
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

pub(crate) fn resolve_ty_path(name: ANameId, context: PathResolutionContext<'_>) -> Option<KTy2> {
    let PathResolutionContext {
        tokens,
        ast,
        name_referents,
        name_symbols,
        k_mod,
        mod_outline,
        mod_outlines,
    } = context;

    let (_, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            return resolve_ty_name(name, name_referents, name_symbols)
                .map(|ty| ty.to_ty2_poly(k_mod, mod_outlines));
        }
    };

    if !tail.is_empty() {
        log::error!("型パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let ty = match resolve_ty_name(name, name_referents, name_symbols)? {
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
            LexicalReferent::Unresolved | LexicalReferent::BuiltInTy(_) => None,
            LexicalReferent::Def => match name_symbols.get(&name)? {
                NameSymbol::TyParam(_) => None,
                NameSymbol::LocalVar(local_var) => Some(KLocalValue::LocalVar(*local_var)),
                NameSymbol::ModSymbol(symbol) => symbol.as_value(mod_outline),
            },
            LexicalReferent::Name(def_name) => match name_symbols.get(&def_name)? {
                NameSymbol::TyParam(_) => None,
                NameSymbol::LocalVar(local_var) => Some(KLocalValue::LocalVar(*local_var)),
                NameSymbol::ModSymbol(symbol) => symbol.as_value(mod_outline),
            },
        })
}

pub(crate) fn resolve_value_path(
    name: ANameId,
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
    let value = match resolve_ty_name(name, name_referents, name_symbols)? {
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

pub(crate) type NameReferents = HashMap<ANameId, LexicalReferent>;

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

pub(crate) type NameResolver = ScopeSystem;

pub(crate) mod v3 {
    use super::*;
    use crate::scope::scope_system::{FindKind, ImportKind};

    pub(crate) fn enter_block(resolver: &mut NameResolver) {
        resolver.enter_scope();
    }

    pub(crate) fn leave_block(resolver: &mut NameResolver) {
        resolver.leave_scope();
    }

    pub(crate) fn on_ty_param_decl(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        // FIXME: 型パラメータの名前の重複はエラーにする
        let text = name.of(ast.names()).text();
        resolver.on_name_def_stacked(name, ImportKind::Ty, text);
    }

    pub(crate) fn on_param_decl(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        // FIXME: パラメータの名前の重複はエラーにする
        let text = name.of(ast.names()).text();
        resolver.on_name_def_stacked(name, ImportKind::Value, text);
    }

    pub(crate) fn on_name_ty(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        log::trace!(
            "on_name_ty {}#{}",
            name.of(ast.names()).head(),
            name.to_index()
        );

        resolver.on_name_use(name, FindKind::Ty, &name.of(ast.names()).head);
    }

    pub(crate) fn on_name_pat(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        let name_data = name.of(ast.names());
        let head = name.of(ast.names()).head();

        // いまのところパス式の末尾以外は型名。
        if name_data.is_qualified() {
            return resolver.on_name_use(name, FindKind::Ty, head);
        }

        // FIXME: const/unit-like struct の可能性もある?
        resolver.on_name_def_stacked(name, ImportKind::Value, head);
    }

    pub(crate) fn on_record_pat(name: ANameId, ast: &ATree, resolver: &mut NameResolver) {
        resolver.on_name_use(name, FindKind::Ty, &name.of(ast.names()).head);
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

        resolver.on_name_use(name, kind, head);
    }

    pub(crate) fn enter_arm(resolver: &mut NameResolver) {
        resolver.enter_scope();
    }

    pub(crate) fn leave_arm(resolver: &mut NameResolver) {
        resolver.leave_scope();
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
            resolver.on_name_def_stacked(name, ImportKind::Value, name.of(ast.names()).text());
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
            resolver.on_name_def_hoisted(name, kind, &name.of(ast.names()).text);
        }
    }

    pub(crate) fn enter_fn_decl(resolver: &mut NameResolver) {
        resolver.enter_scope();
    }

    pub(crate) fn leave_fn_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        resolver.leave_scope();
        leave_hoisted_decl("fn", name_opt, ImportKind::Value, ast, resolver);
    }

    pub(crate) fn enter_extern_fn_decl(resolver: &mut NameResolver) {
        resolver.enter_scope();
    }

    pub(crate) fn leave_extern_fn_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        resolver.leave_scope();
        leave_hoisted_decl("extern fn", name_opt, ImportKind::Value, ast, resolver);
    }

    pub(crate) fn enter_enum_decl(resolver: &mut NameResolver) {
        resolver.enter_scope();
    }

    pub(crate) fn leave_enum_decl(
        name_opt: Option<ANameId>,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        resolver.leave_scope();
        leave_hoisted_decl("enum", name_opt, ImportKind::Ty, ast, resolver);
    }

    pub(crate) fn enter_struct_decl(resolver: &mut NameResolver) {
        resolver.enter_scope();
    }

    pub(crate) fn leave_struct_decl(
        name_opt: Option<ANameId>,
        is_unit_like: bool,
        ast: &ATree,
        resolver: &mut NameResolver,
    ) {
        resolver.leave_scope();

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
