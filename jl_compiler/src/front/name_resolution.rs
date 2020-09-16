//! 名前解決の処理

use crate::{cps::*, front::*, scope::lexical_referent::LexicalReferent};
use std::collections::HashMap;

pub(crate) struct PathResolutionContext<'a> {
    pub(super) tokens: &'a PTokens,
    pub(super) ast: &'a ATree,
    pub(super) name_referents: &'a NameReferents,
    pub(super) name_symbols: &'a NameSymbols,
    pub(super) mod_outline: &'a KModOutline,
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
        mod_outline,
    } = context;

    let (_, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            return resolve_ty_name(name, name_referents, name_symbols)
                .map(|ty| ty.to_ty2_poly(mod_outline));
        }
    };

    if !tail.is_empty() {
        log::error!("型パスは <enumの名前>::<バリアントの名前> の形以外未実装です");
        return None;
    }

    // モジュール名を含むパスは未実装なので <enum名>::<バリアント> の形しかない。
    let ty = match resolve_ty_name(name, name_referents, name_symbols)? {
        KTy::Alias(alias) => match alias.of(&mod_outline.aliases).referent_as_ty() {
            Some(KTy2::StructEnum(struct_enum)) => {
                let name = name.of(ast.names()).token.text(tokens);
                let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

                KTy2::Struct(k_struct)
            }
            _ => return None,
        },
        KTy::StructEnum(struct_enum) => {
            let name = name.of(ast.names()).token.text(tokens);
            let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

            KTy2::Struct(k_struct)
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
        mod_outline,
    } = context;

    let (_, tail) = match name.of(ast.names()).quals.split_first() {
        Some(it) => it,
        None => {
            let value = resolve_value_name(name, name_referents, name_symbols, mod_outline)?;
            return Some(KProjectValue::new(MOD, value));
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
                    let k_const = find_const_variant(const_enum, name, mod_outline)?;
                    KProjectValue::new(k_mod, KLocalValue::Const(k_const))
                }
                KProjectSymbol::StructEnum(struct_enum) => {
                    let k_struct = find_struct_variant(struct_enum, name, mod_outline)?;

                    if k_struct.of(&mod_outline.structs).is_unit_like() {
                        KProjectValue::new(MOD, KLocalValue::UnitLikeStruct(k_struct))
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
    Some(KProjectValue::new(MOD, value))
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
