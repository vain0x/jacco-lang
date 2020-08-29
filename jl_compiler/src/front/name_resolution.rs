//! 名前解決の処理

use crate::{cps::*, front::env::Env, front::*, utils::VecArena};

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
        KModSymbol::Const(_)
        | KModSymbol::StaticVar(_)
        | KModSymbol::Fn(_)
        | KModSymbol::ExternFn(_)
        | KModSymbol::Field(_) => return,
        KModSymbol::ConstEnum(const_enum) => KTy::ConstEnum(const_enum),
        KModSymbol::StructEnum(struct_enum) => KTy::StructEnum(struct_enum),
        KModSymbol::Struct(k_struct) => KTy::Struct(k_struct),
        KModSymbol::Alias(alias) => KTy::Alias(alias),
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
        KModSymbol::Alias(alias) => KLocalValue::Alias(alias),
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
    path: &AName,
    key: ANameKey,
    context: PathResolutionContext<'_>,
) -> Option<KTy2> {
    let PathResolutionContext {
        tokens,
        k_mod,
        mod_outline,
        mod_outlines,
        env,
        listener,
    } = context;

    let (head, tail) = match path.quals.split_first() {
        Some(it) => it,
        None => {
            return resolve_ty_name(&path.text, key, env, listener)
                .map(|ty| ty.to_ty2_poly(k_mod, mod_outlines));
        }
    };

    if !tail.is_empty() {
        log::error!("型パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let ty = match resolve_ty_name(head.text(tokens), key, env, listener)? {
        KTy::StructEnum(struct_enum) => {
            let name = path.token.text(tokens);
            let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

            KTy2::Struct(k_mod, k_struct)
        }
        KTy::Alias(alias) => match alias.of(&mod_outline.aliases).referent_as_ty() {
            Some(KTy2::StructEnum(KProjectStructEnum(k_mod, struct_enum))) => {
                let name = path.token.text(tokens);
                let k_struct = find_struct_variant(struct_enum, name, k_mod.of(&mod_outlines))?;

                KTy2::Struct(k_mod, k_struct)
            }
            _ => return None,
        },
        _ => return None,
    };
    Some(ty)
}

fn resolve_value_name(name: &str, env: &Env) -> Option<KLocalValue> {
    env.find_value(name)
}

pub(crate) fn resolve_value_path(
    path: &AName,
    key: ANameKey,
    context: PathResolutionContext<'_>,
) -> Option<KProjectValue> {
    let PathResolutionContext {
        tokens,
        k_mod,
        mod_outline,
        mod_outlines,
        env,
        listener,
    } = context;

    let (head, tail) = match path.quals.split_first() {
        Some(it) => it,
        None => {
            let value = resolve_value_name(&path.text, env)?;
            return Some(KProjectValue::new(k_mod, value));
        }
    };

    if !tail.is_empty() {
        log::error!("パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let value = match resolve_ty_name(head.text(tokens), key, env, listener)? {
        KTy::ConstEnum(const_enum) => {
            let name = path.token.text(tokens);
            let k_const = find_const_variant(const_enum, name, mod_outline)?;
            KLocalValue::Const(k_const)
        }
        KTy::StructEnum(struct_enum) => {
            let name = path.token.text(tokens);
            let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

            if k_struct.of(&mod_outline.structs).is_unit_like() {
                KLocalValue::UnitLikeStruct(k_struct)
            } else {
                return None;
            }
        }
        KTy::Alias(alias) => {
            let name = path.token.text(tokens);
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
        _ => return None,
    };
    Some(KProjectValue::new(k_mod, value))
}
