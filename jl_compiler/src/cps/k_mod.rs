use super::*;
use crate::{logs::Logger, utils::VecArena, utils::VecArenaId};

pub(crate) struct KModTag;

pub(crate) type KMod = VecArenaId<KModTag>;

pub(crate) type KModInfoArena = VecArena<KModTag, KModInfo>;

#[allow(unused)]
#[derive(Default)]
pub(crate) struct KModInfo {
    pub(crate) name: String,
}

#[derive(Default)]
pub(crate) struct KModOutline {
    pub(crate) mods: KModInfoArena,
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

impl KModOutline {
    pub(crate) fn symbol_count(&self) -> usize {
        self.mods.len()
            + self.aliases.len()
            + self.consts.len()
            + self.static_vars.len()
            + self.fns.len()
            + self.extern_fns.len()
            + self.const_enums.len()
            + self.struct_enums.len()
            + self.structs.len()
            + self.fields.len()
    }
}

#[derive(Default)]
pub(crate) struct KModData {
    pub(crate) consts: KConstInits,
    pub(crate) static_vars: KStaticVarInits,
    pub(crate) fns: KFnArena,
    pub(crate) extern_fns: KExternFnArena,
}

/// モジュールの中で定義されるシンボルの識別子。それが属するモジュールを基準としている。
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum KModSymbol {
    Alias(KAlias),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    ConstEnum(KConstEnum),
    StructEnum(KStructEnum),
    Struct(KStruct),
    #[allow(unused)]
    Field(KField),
}

impl KModSymbol {
    #[cfg(unused)]
    pub(crate) fn name(self, mod_outline: &KModOutline) -> &str {
        match self.outline(mod_outline) {
            KModSymbolRef::Alias(_, alias_data) => alias_data.name(),
            KModSymbolRef::Const(_, const_outline) => &const_outline.name,
            KModSymbolRef::StaticVar(_, static_var_outline) => &static_var_outline.name,
            KModSymbolRef::Fn(_, fn_data) => &fn_data.name,
            KModSymbolRef::ExternFn(_, extern_fn_data) => &extern_fn_data.name,
            KModSymbolRef::ConstEnum(_, const_enum_data) => &const_enum_data.name,
            KModSymbolRef::StructEnum(_, enum_data) => &enum_data.name,
            KModSymbolRef::Struct(_, struct_data) => &struct_data.name,
            KModSymbolRef::Field(_, field_outline) => &field_outline.name,
        }
    }

    pub(crate) fn as_ty(self) -> Option<KTy> {
        let ty = match self {
            KModSymbol::Alias(alias) => KTy::Alias(alias),
            KModSymbol::Const(_)
            | KModSymbol::StaticVar(_)
            | KModSymbol::Fn(_)
            | KModSymbol::ExternFn(_)
            | KModSymbol::Field(_) => return None,
            KModSymbol::ConstEnum(const_enum) => KTy::ConstEnum(const_enum),
            KModSymbol::StructEnum(struct_enum) => KTy::StructEnum(struct_enum),
            KModSymbol::Struct(k_struct) => KTy::Struct(k_struct),
        };
        Some(ty)
    }

    pub(crate) fn as_value(self, mod_outline: &KModOutline) -> Option<KValueOrAlias> {
        let value = match self {
            KModSymbol::Alias(alias) => return Some(KValueOrAlias::Alias(alias)),
            KModSymbol::Const(k_const) => KModValue::Const(k_const),
            KModSymbol::StaticVar(static_var) => KModValue::StaticVar(static_var),
            KModSymbol::Fn(k_fn) => KModValue::Fn(k_fn),
            KModSymbol::ExternFn(extern_fn) => KModValue::ExternFn(extern_fn),
            KModSymbol::ConstEnum(_) | KModSymbol::StructEnum(_) | KModSymbol::Field(_) => {
                return None;
            }
            KModSymbol::Struct(k_struct) => {
                if k_struct.of(&mod_outline.structs).is_unit_like() {
                    KModValue::UnitLikeStruct(k_struct)
                } else {
                    return None;
                }
            }
        };
        Some(KValueOrAlias::Value(value))
    }
}

pub(crate) fn resolve_aliases(
    aliases: &mut KAliasArena,
    mod_outline: &KModOutline,
    logger: Logger,
) {
    // FIXME: use の先頭を処理する。

    for alias_data in aliases.iter_mut() {
        let entity_name = match alias_data.path() {
            [_, entity_name] => entity_name,
            _ => {
                logger.error(
                    alias_data.loc(),
                    "use mod_name::entity_name; 以外の形式の use は未実装です",
                );
                continue;
            }
        };

        let lookup =
            |name: &str| -> Option<KModSymbol> {
                mod_outline
                    .consts
                    .enumerate()
                    .map(|(id, outline)| (outline.name.as_str(), KModSymbol::Const(id)))
                    .chain(
                        mod_outline.static_vars.enumerate().map(|(id, outline)| {
                            (outline.name.as_str(), KModSymbol::StaticVar(id))
                        }),
                    )
                    .chain(
                        mod_outline
                            .fns
                            .enumerate()
                            .map(|(id, outline)| (outline.name.as_str(), KModSymbol::Fn(id))),
                    )
                    .chain(
                        mod_outline
                            .extern_fns
                            .enumerate()
                            .map(|(id, outline)| (outline.name.as_str(), KModSymbol::ExternFn(id))),
                    )
                    .chain(
                        mod_outline.const_enums.enumerate().map(|(id, outline)| {
                            (outline.name.as_str(), KModSymbol::ConstEnum(id))
                        }),
                    )
                    .chain(
                        mod_outline.struct_enums.enumerate().map(|(id, outline)| {
                            (outline.name.as_str(), KModSymbol::StructEnum(id))
                        }),
                    )
                    .chain(
                        mod_outline
                            .structs
                            .enumerate()
                            .map(|(id, outline)| (outline.name.as_str(), KModSymbol::Struct(id))),
                    )
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

mod debug_info {
    #![allow(unused)]

    use super::*;
    use std::{cell::RefCell, ptr::null};

    thread_local! {
        static MOD_OUTLINE: RefCell<*const KModOutline> = RefCell::new(null());
    }

    impl KModOutline {
        pub(crate) fn given_for_debug<T>(mod_outline: &KModOutline, f: impl FnOnce() -> T) -> T {
            if cfg!(debug_assertions) {
                MOD_OUTLINE.with(|slot| {
                    *slot.borrow_mut() = mod_outline as *const KModOutline;
                    let result = f();
                    *slot.borrow_mut() = null();
                    result
                })
            } else {
                f()
            }
        }

        pub(crate) fn using_for_debug<T>(f: impl FnOnce(Option<&KModOutline>) -> T) -> T {
            if cfg!(debug_assertions) {
                MOD_OUTLINE.with(|slot| {
                    let ptr = slot.borrow();
                    let ref_opt = if ptr.is_null() {
                        None
                    } else {
                        Some(unsafe { &**ptr })
                    };

                    f(ref_opt)
                })
            } else {
                f(None)
            }
        }
    }
}
