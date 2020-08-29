use super::*;
use crate::{
    logs::Logger,
    utils::{VecArena, VecArenaId},
};
use std::collections::HashMap;

pub(crate) struct KModTag;

pub(crate) type KMod = VecArenaId<KModTag>;

pub(crate) type KModOutlines = VecArena<KModTag, KModOutline>;

pub(crate) type KModArena = VecArena<KModTag, KModData>;

#[derive(Default)]
pub(crate) struct KModOutline {
    pub(crate) name: String,
    pub(crate) aliases: KAliasArena,
    pub(crate) consts: KConstOutlineArena,
    pub(crate) static_vars: KStaticVarOutlineArena,
    pub(crate) fns: KFnOutlineArena,
    pub(crate) extern_fns: KExternFnOutlineArena,
    pub(crate) const_enums: KConstEnumOutlines,
    pub(crate) struct_enums: KStructEnumArena,
    pub(crate) structs: KStructArena,
    pub(crate) fields: KFieldArena,
}

#[derive(Debug, Default)]
pub(crate) struct KModData {
    pub(crate) consts: KConstInits,
    pub(crate) static_vars: KStaticVarInits,
    pub(crate) fns: KFnArena,
    pub(crate) extern_fns: KExternFnArena,
}

/// モジュールの中で定義されるシンボルの識別子。それが属するモジュールを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KModSymbol {
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    ConstEnum(KConstEnum),
    StructEnum(KStructEnum),
    Struct(KStruct),
    #[allow(unused)]
    Field(KField),
    Alias(KAlias),
}

impl KModSymbol {
    pub(crate) fn outline(self, mod_outline: &KModOutline) -> KModLocalSymbolOutline<'_> {
        match self {
            KModSymbol::Alias(alias) => {
                KModLocalSymbolOutline::Alias(alias, alias.of(&mod_outline.aliases))
            }
            KModSymbol::Const(k_const) => {
                KModLocalSymbolOutline::Const(k_const, k_const.of(&mod_outline.consts))
            }
            KModSymbol::StaticVar(static_var) => KModLocalSymbolOutline::StaticVar(
                static_var,
                static_var.of(&mod_outline.static_vars),
            ),
            KModSymbol::Fn(k_fn) => KModLocalSymbolOutline::Fn(k_fn, k_fn.of(&mod_outline.fns)),
            KModSymbol::ExternFn(extern_fn) => {
                KModLocalSymbolOutline::ExternFn(extern_fn, extern_fn.of(&mod_outline.extern_fns))
            }
            KModSymbol::ConstEnum(const_enum) => KModLocalSymbolOutline::ConstEnum(
                const_enum,
                const_enum.of(&mod_outline.const_enums),
            ),
            KModSymbol::StructEnum(struct_enum) => KModLocalSymbolOutline::StructEnum(
                struct_enum,
                struct_enum.of(&mod_outline.struct_enums),
            ),
            KModSymbol::Struct(k_struct) => {
                KModLocalSymbolOutline::Struct(k_struct, k_struct.of(&mod_outline.structs))
            }
            KModSymbol::Field(k_field) => {
                KModLocalSymbolOutline::Field(k_field, k_field.of(&mod_outline.fields))
            }
        }
    }

    pub(crate) fn name(self, mod_outline: &KModOutline) -> &str {
        match self.outline(mod_outline) {
            KModLocalSymbolOutline::Const(_, const_outline) => &const_outline.name,
            KModLocalSymbolOutline::StaticVar(_, static_var_outline) => &static_var_outline.name,
            KModLocalSymbolOutline::Fn(_, fn_data) => &fn_data.name,
            KModLocalSymbolOutline::ExternFn(_, extern_fn_data) => &extern_fn_data.name,
            KModLocalSymbolOutline::ConstEnum(_, const_enum_data) => &const_enum_data.name,
            KModLocalSymbolOutline::StructEnum(_, enum_data) => &enum_data.name,
            KModLocalSymbolOutline::Struct(_, struct_data) => &struct_data.name,
            KModLocalSymbolOutline::Field(_, field_outline) => &field_outline.name,
            KModLocalSymbolOutline::Alias(_, alias_data) => alias_data.name(),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum KModLocalSymbolOutline<'a> {
    Const(KConst, &'a KConstOutline),
    StaticVar(KStaticVar, &'a KStaticVarOutline),
    Fn(KFn, &'a KFnOutline),
    ExternFn(KExternFn, &'a KExternFnOutline),
    ConstEnum(KConstEnum, &'a KConstEnumOutline),
    StructEnum(KStructEnum, &'a KStructEnumOutline),
    Struct(KStruct, &'a KStructOutline),
    Field(KField, &'a KFieldOutline),
    Alias(KAlias, &'a KAliasOutline),
}

/// プロジェクト内で定義されるシンボルの名前。それが属するプロジェクトを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KProjectSymbol {
    #[allow(unused)]
    Mod(KMod),
    StaticVar(KProjectStaticVar),
    Const(KProjectConst),
    Fn(KProjectFn),
    ExternFn(KProjectExternFn),
    ConstEnum(KProjectConstEnum),
    StructEnum(KProjectStructEnum),
    Struct(KProjectStruct),
}

impl KProjectSymbol {
    pub(crate) fn outline<'a>(self, mod_outlines: &'a KModOutlines) -> KProjectSymbolOutline<'a> {
        match self {
            KProjectSymbol::Mod(k_mod) => KProjectSymbolOutline::Mod(k_mod, k_mod.of(mod_outlines)),
            KProjectSymbol::Const(k_const) => {
                KProjectSymbolOutline::Const(k_const.k_mod(), k_const.of(mod_outlines))
            }
            KProjectSymbol::StaticVar(static_var) => {
                KProjectSymbolOutline::StaticVar(static_var.k_mod(), static_var.of(mod_outlines))
            }
            KProjectSymbol::Fn(k_fn) => {
                KProjectSymbolOutline::Fn(k_fn.k_mod(), k_fn.of(mod_outlines))
            }
            KProjectSymbol::ExternFn(extern_fn) => {
                KProjectSymbolOutline::ExternFn(extern_fn.k_mod(), extern_fn.of(mod_outlines))
            }
            KProjectSymbol::ConstEnum(const_enum) => {
                KProjectSymbolOutline::ConstEnum(const_enum.of(mod_outlines))
            }
            KProjectSymbol::StructEnum(struct_enum) => {
                KProjectSymbolOutline::StructEnum(struct_enum.of(mod_outlines))
            }
            KProjectSymbol::Struct(k_struct) => {
                KProjectSymbolOutline::Struct(k_struct.of(mod_outlines))
            }
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum KProjectSymbolOutline<'a> {
    Mod(KMod, &'a KModOutline),
    Const(KMod, &'a KConstOutline),
    StaticVar(KMod, &'a KStaticVarOutline),
    Fn(KMod, &'a KFnOutline),
    ExternFn(KMod, &'a KExternFnOutline),
    ConstEnum(&'a KConstEnumOutline),
    StructEnum(&'a KStructEnumOutline),
    Struct(&'a KStructOutline),
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

        let lookup = |name: &str| -> Option<KProjectSymbol> {
            mod_outline
                .consts
                .enumerate()
                .map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::Const(KProjectConst(k_mod, id)),
                    )
                })
                .chain(mod_outline.static_vars.enumerate().map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::StaticVar(KProjectStaticVar(k_mod, id)),
                    )
                }))
                .chain(mod_outline.fns.enumerate().map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::Fn(KProjectFn(k_mod, id)),
                    )
                }))
                .chain(mod_outline.extern_fns.enumerate().map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::ExternFn(KProjectExternFn(k_mod, id)),
                    )
                }))
                .chain(mod_outline.const_enums.enumerate().map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::ConstEnum(KProjectConstEnum(k_mod, id)),
                    )
                }))
                .chain(mod_outline.struct_enums.enumerate().map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::StructEnum(KProjectStructEnum(k_mod, id)),
                    )
                }))
                .chain(mod_outline.structs.enumerate().map(|(id, outline)| {
                    (
                        outline.name.as_str(),
                        KProjectSymbol::Struct(KProjectStruct(k_mod, id)),
                    )
                }))
                .find_map(
                    |(the_name, symbol)| {
                        if the_name == name {
                            Some(symbol)
                        } else {
                            None
                        }
                    },
                )
        };

        let referent = match lookup(entity_name.as_str()) {
            Some(it) => it,
            None => {
                logger.error(alias_data.loc(), "名前が見つかりません");
                continue;
            }
        };

        // FIXME: privacy 検査
        alias_data.bind(referent);
    }
}
