use super::{
    k_const::KConstArena, k_enum::KEnumArena, k_extern_fn::KExternFnOutlineArena,
    k_fn::KFnOutlineArena, k_static_var::KStaticVarArena, k_struct::KStructArena, KAlias,
    KAliasArena, KConst, KEnum, KExternFn, KExternFnArena, KFieldArena, KFn, KFnArena, KLocal,
    KStaticVar, KStruct,
};
use crate::{
    logs::Logger,
    utils::{VecArena, VecArenaId},
};
use std::collections::HashMap;

pub(crate) struct KModTag;

#[allow(unused)]
pub(crate) type KMod = VecArenaId<KModTag>;

#[allow(unused)]
pub(crate) type KModOutlines = VecArena<KModTag, KModOutline>;

#[allow(unused)]
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

#[allow(unused)]
/// モジュールの中で定義されるシンボルの識別子。それが属するモジュールを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KModLocalSymbol {
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
    Alias(KAlias),
}

#[allow(unused)]
/// プロジェクト内で定義されるシンボルの名前。それが属するプロジェクトを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KProjectSymbol {
    Mod(KMod),
    ModLocal {
        k_mod: KMod,
        symbol: KModLocalSymbol,
    },
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
            [] => continue,
            [mod_name, entity_name] => (mod_name, entity_name),
            _ => {
                logger.error(
                    alias_data.location(),
                    "use mod_name::entity_name; 以外の形式の use は未実装です",
                );
                continue;
            }
        };

        let k_mod = match mod_map.get(mod_name.as_str()) {
            Some(k_mod) => *k_mod,
            None => {
                logger.error(alias_data.location(), "モジュール名が不明です");
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
                logger.error(alias_data.location(), "名前が見つかりません");
                continue;
            }
        };
        alias_data.bind(referent);
    }
}
