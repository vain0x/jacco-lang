use super::{
    k_const::KConstArena,
    k_enum::{KEnumArena, KEnumReprs},
    k_extern_fn::KExternFnOutlineArena,
    k_fn::KFnOutlineArena,
    k_static_var::KStaticVarArena,
    k_struct::KStructArena,
    KAlias, KAliasArena, KAliasOutline, KConst, KConstData, KEnum, KEnumOutline, KExternFn,
    KExternFnArena, KExternFnOutline, KFieldArena, KFn, KFnArena, KFnOutline, KLabelArena, KLocal,
    KLocalArena, KStaticVar, KStaticVarData, KStruct, KStructOutline,
};
use crate::{
    logs::Logger,
    utils::{VecArena, VecArenaId},
};
use std::collections::HashMap;

pub(crate) struct KModTag;

pub(crate) type KMod = VecArenaId<KModTag>;

pub(crate) type KModOutlines = VecArena<KModTag, KModOutline>;

pub(crate) type KModArena = VecArena<KModTag, KModData>;

#[derive(Debug, Default)]
pub(crate) struct KModOutline {
    pub(crate) name: String,
    pub(crate) aliases: KAliasArena,
    pub(crate) consts: KConstArena,
    pub(crate) static_vars: KStaticVarArena,
    pub(crate) fns: KFnOutlineArena,
    pub(crate) extern_fns: KExternFnOutlineArena,
    pub(crate) enums: KEnumArena,
    pub(crate) enum_reprs: KEnumReprs,
    pub(crate) structs: KStructArena,
    pub(crate) fields: KFieldArena,
}

#[derive(Debug, Default)]
pub(crate) struct KModData {
    pub(crate) fns: KFnArena,
    pub(crate) extern_fns: KExternFnArena,
}

#[allow(unused)]
/// ローカル変数の親
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KLocalVarParent {
    Fn(KFn),
    ExternFn(KExternFn),
}

impl KLocalVarParent {
    pub(crate) fn locals(self, mod_data: &KModData) -> &KLocalArena {
        match self {
            KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).locals,
            KLocalVarParent::ExternFn(extern_fn) => &extern_fn.of(&mod_data.extern_fns).locals,
        }
    }

    pub(crate) fn labels(self, mod_data: &KModData) -> &KLabelArena {
        match self {
            KLocalVarParent::Fn(k_fn) => &k_fn.of(&mod_data.fns).labels,
            KLocalVarParent::ExternFn(_) => KLabelArena::EMPTY,
        }
    }
}

/// モジュールの中で定義されるシンボルの識別子。それが属するモジュールを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KModLocalSymbol {
    #[allow(unused)]
    LocalVar {
        parent: KLocalVarParent,
        local_var: KLocal,
    },
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    Enum(KEnum),
    Struct(KStruct),
    #[allow(unused)]
    Alias(KAlias),
}

impl KModLocalSymbol {
    pub(crate) fn outline(self, mod_outline: &KModOutline) -> KModLocalSymbolOutline<'_> {
        match self {
            KModLocalSymbol::Alias(alias) => {
                KModLocalSymbolOutline::Alias(alias, alias.of(&mod_outline.aliases))
            }
            KModLocalSymbol::LocalVar { parent, local_var } => {
                KModLocalSymbolOutline::LocalVar(parent, local_var)
            }
            KModLocalSymbol::Const(k_const) => {
                KModLocalSymbolOutline::Const(k_const, k_const.of(&mod_outline.consts))
            }
            KModLocalSymbol::StaticVar(static_var) => KModLocalSymbolOutline::StaticVar(
                static_var,
                static_var.of(&mod_outline.static_vars),
            ),
            KModLocalSymbol::Fn(k_fn) => {
                KModLocalSymbolOutline::Fn(k_fn, k_fn.of(&mod_outline.fns))
            }
            KModLocalSymbol::ExternFn(extern_fn) => {
                KModLocalSymbolOutline::ExternFn(extern_fn, extern_fn.of(&mod_outline.extern_fns))
            }
            KModLocalSymbol::Enum(k_enum) => {
                KModLocalSymbolOutline::Enum(k_enum, k_enum.of(&mod_outline.enums))
            }
            KModLocalSymbol::Struct(k_struct) => {
                KModLocalSymbolOutline::Struct(k_struct, k_struct.of(&mod_outline.structs))
            }
        }
    }

    #[allow(unused)]
    pub(crate) fn name(self, mod_outline: &KModOutline) -> Option<&str> {
        let name = match self.outline(mod_outline) {
            KModLocalSymbolOutline::LocalVar(..) => return None,
            KModLocalSymbolOutline::Const(_, const_data) => &const_data.name,
            KModLocalSymbolOutline::StaticVar(_, static_var_data) => &static_var_data.name,
            KModLocalSymbolOutline::Fn(_, fn_data) => &fn_data.name,
            KModLocalSymbolOutline::ExternFn(_, extern_fn_data) => &extern_fn_data.name,
            KModLocalSymbolOutline::Enum(_, enum_data) => &enum_data.name,
            KModLocalSymbolOutline::Struct(_, struct_data) => &struct_data.name,
            KModLocalSymbolOutline::Alias(_, alias_data) => alias_data.name(),
        };
        Some(name)
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum KModLocalSymbolOutline<'a> {
    LocalVar(KLocalVarParent, KLocal),
    Const(KConst, &'a KConstData),
    StaticVar(KStaticVar, &'a KStaticVarData),
    Fn(KFn, &'a KFnOutline),
    ExternFn(KExternFn, &'a KExternFnOutline),
    Enum(KEnum, &'a KEnumOutline),
    Struct(KStruct, &'a KStructOutline),
    Alias(KAlias, &'a KAliasOutline),
}

/// プロジェクト内で定義されるシンボルの名前。それが属するプロジェクトを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KProjectSymbol {
    #[allow(unused)]
    Mod(KMod),
    ModLocal {
        k_mod: KMod,
        symbol: KModLocalSymbol,
    },
}

impl KProjectSymbol {
    pub(crate) fn outline<'a>(self, mod_outlines: &'a KModOutlines) -> KProjectSymbolOutline<'a> {
        let (k_mod, symbol) = match self {
            KProjectSymbol::ModLocal { k_mod, symbol } => (k_mod, symbol),
            KProjectSymbol::Mod(k_mod) => {
                return KProjectSymbolOutline::Mod(k_mod, k_mod.of(mod_outlines));
            }
        };

        let mod_outline = k_mod.of(mod_outlines);
        let symbol_outline = symbol.outline(mod_outline);
        KProjectSymbolOutline::ModLocal {
            k_mod,
            mod_outline,
            symbol_outline,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum KProjectSymbolOutline<'a> {
    Mod(KMod, &'a KModOutline),
    ModLocal {
        k_mod: KMod,
        mod_outline: &'a KModOutline,
        symbol_outline: KModLocalSymbolOutline<'a>,
    },
}

impl KAliasOutline {
    pub(crate) fn referent_outline<'a>(
        &self,
        mod_outlines: &'a KModOutlines,
    ) -> Option<KProjectSymbolOutline<'a>> {
        self.referent().map(|symbol| symbol.outline(mod_outlines))
    }
}

pub(crate) fn resolve_aliases(
    aliases: &mut KAliasArena,
    mod_outlines: &KModOutlines,
    logger: Logger,
) {
    let mod_map = mod_outlines
        .enumerate()
        .map(|(k_mod, mod_outline)| (mod_outline.name.to_string(), k_mod))
        .collect::<HashMap<_, _>>();

    for alias_data in aliases.iter_mut() {
        let (mod_name, entity_name) = match alias_data.path() {
            [mod_name, entity_name] => (mod_name, entity_name),
            _ => {
                logger.error(
                    alias_data.loc(),
                    "use mod_name::entity_name; 以外の形式の use は未実装です",
                );
                continue;
            }
        };

        let k_mod = match mod_map.get(mod_name.as_str()) {
            Some(k_mod) => *k_mod,
            None => {
                logger.error(alias_data.loc(), "モジュール名が不明です");
                continue;
            }
        };

        let mod_outline = k_mod.of(mod_outlines);

        let lookup =
            |name: &str| -> Option<KModLocalSymbol> {
                mod_outline
                    .consts
                    .enumerate()
                    .map(|(id, outline)| (outline.name.as_str(), KModLocalSymbol::Const(id)))
                    .chain(mod_outline.static_vars.enumerate().map(|(id, outline)| {
                        (outline.name.as_str(), KModLocalSymbol::StaticVar(id))
                    }))
                    .chain(
                        mod_outline
                            .fns
                            .enumerate()
                            .map(|(id, outline)| (outline.name.as_str(), KModLocalSymbol::Fn(id))),
                    )
                    .chain(mod_outline.extern_fns.enumerate().map(|(id, outline)| {
                        (outline.name.as_str(), KModLocalSymbol::ExternFn(id))
                    }))
                    .chain(
                        mod_outline.enums.enumerate().map(|(id, outline)| {
                            (outline.name.as_str(), KModLocalSymbol::Enum(id))
                        }),
                    )
                    .chain(
                        mod_outline.structs.enumerate().map(|(id, outline)| {
                            (outline.name.as_str(), KModLocalSymbol::Struct(id))
                        }),
                    )
                    .find_map(
                        |(the_name, symbol)| if the_name == name { Some(symbol) } else { None },
                    )
            };

        let referent = match lookup(entity_name.as_str()) {
            Some(symbol) => KProjectSymbol::ModLocal { k_mod, symbol },
            None => {
                logger.error(alias_data.loc(), "名前が見つかりません");
                continue;
            }
        };

        // FIXME: privacy 検査
        alias_data.bind(referent);
    }
}
