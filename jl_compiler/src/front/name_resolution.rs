//! 名前解決の処理

use crate::{cps::*, front::env::Env, front::*, source::Loc, utils::VecArena};

// =============================================================================
// V2
// =============================================================================

pub(crate) type DeclSymbols = VecArena<ADeclTag, Option<KModLocalSymbol>>;

#[derive(Copy, Clone)]
pub(crate) enum KLocalValue {
    LocalVar(KLocal),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct(KStruct),
    Alias(KAlias),
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
) -> Option<(String, KModLocalSymbol)> {
    let symbol = decl_symbols.get(decl_id).copied().flatten()?;
    let name = symbol.name(mod_outline)?;
    Some((name.to_string(), symbol))
}

pub(crate) fn do_add_ty_symbol_to_local_env(name: &str, symbol: KModLocalSymbol, env: &mut Env) {
    let ty = match symbol {
        KModLocalSymbol::LocalVar { .. }
        | KModLocalSymbol::Const(_)
        | KModLocalSymbol::StaticVar(_)
        | KModLocalSymbol::Fn(_)
        | KModLocalSymbol::ExternFn(_) => return,
        KModLocalSymbol::Struct(k_struct) => KTy::Struct(k_struct),
        KModLocalSymbol::Enum(k_enum) => KTy::Enum(k_enum),
        KModLocalSymbol::Alias(alias) => KTy::Alias(alias),
    };

    env.insert_ty(name.to_string(), ty);
}

fn do_add_value_symbol_to_local_env(
    name: &str,
    symbol: KModLocalSymbol,
    mod_outline: &KModOutline,
    env: &mut Env,
) {
    let value = match symbol {
        KModLocalSymbol::LocalVar { local_var, .. } => KLocalValue::LocalVar(local_var),
        KModLocalSymbol::Const(k_const) => KLocalValue::Const(k_const),
        KModLocalSymbol::StaticVar(static_var) => KLocalValue::StaticVar(static_var),
        KModLocalSymbol::Fn(k_fn) => KLocalValue::Fn(k_fn),
        KModLocalSymbol::ExternFn(extern_fn) => KLocalValue::ExternFn(extern_fn),
        KModLocalSymbol::Struct(k_struct) if k_struct.of(&mod_outline.structs).is_unit_like() => {
            KLocalValue::UnitLikeStruct(k_struct)
        }
        KModLocalSymbol::Struct(_) | KModLocalSymbol::Enum(_) => return,
        KModLocalSymbol::Alias(alias) => KLocalValue::Alias(alias),
    };

    env.insert_value(name.to_string(), value)
}

fn add_symbol_to_local_env(
    name: &str,
    symbol: KModLocalSymbol,
    mod_outline: &KModOutline,
    env: &mut Env,
) {
    do_add_ty_symbol_to_local_env(name, symbol, env);
    do_add_value_symbol_to_local_env(name, symbol, mod_outline, env);
}

fn add_variant_symbols_to_local_env(k_enum: KEnum, mod_outline: &KModOutline, env: &mut Env) {
    let enum_name = k_enum.name(&mod_outline.enums);

    for variant in k_enum.variants(&mod_outline.enums) {
        let symbol = KModLocalSymbol::from_variant(*variant);
        // FIXME: `::` が名前空間を辿るようにする
        let full_name = format!(
            "{}::{}",
            enum_name,
            variant.name(&mod_outline.consts, &mod_outline.structs)
        );
        add_symbol_to_local_env(&full_name, symbol, mod_outline, env);
    }
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
        if let KModLocalSymbol::Enum(k_enum) = symbol {
            add_variant_symbols_to_local_env(k_enum, mod_outline, env);
        }

        add_symbol_to_local_env(&name, symbol, mod_outline, env);
    }
}

// -----------------------------------------------
// 型
// -----------------------------------------------

fn resolve_builtin_ty_name(name: &str) -> Option<KTy> {
    KNumberTy::parse(name).map(KTy::Number).or_else(|| {
        if name == "never" {
            Some(KTy::Never)
        } else {
            None
        }
    })
}

pub(crate) fn resolve_ty_name(name: &str, env: &Env) -> Option<KTy> {
    env.find_ty(name)
        .cloned()
        .or_else(|| resolve_builtin_ty_name(name))
}

pub(crate) fn resolve_value_name(name: &str, _loc: Loc, env: &Env) -> Option<KLocalValue> {
    env.find_value(name)
}
