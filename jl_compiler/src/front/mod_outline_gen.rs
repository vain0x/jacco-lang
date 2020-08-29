use super::{
    cps_conversion::{convert_ty, convert_ty_opt, TyResolver},
    env::Env,
    name_resolution::{do_add_ty_symbol_to_local_env, DeclSymbols, NameResolutionListener},
};
use crate::{
    cps::*,
    logs::DocLogger,
    parse::*,
    source::{Doc, Loc},
};
use std::iter::once;

fn resolve_modifiers(modifiers: &ADeclModifiers) -> Option<KVis> {
    modifiers.vis_opt
}

fn resolve_name_opt(name_opt: Option<&AName>) -> String {
    name_opt.map_or(String::new(), |name| name.text.to_string())
}

fn resolve_ty_opt(ty_opt: Option<ATyId>, ty_resolver: &mut TyResolver) -> KTy {
    convert_ty_opt(ty_opt, ty_resolver)
}

fn resolve_ty_or_unit(ty_opt: Option<ATyId>, ty_resolver: &mut TyResolver) -> KTy {
    match ty_opt {
        Some(ty) => convert_ty(ty, ty_resolver),
        None => KTy::Unit,
    }
}

fn add_ty_params_to_env(
    ty_params: &[ATyParamDecl],
    doc: Doc,
    decl_id: ADeclId,
    ty_resolver: &mut TyResolver,
) {
    for ty_param in ty_params {
        let name = ty_param.name.text().to_string();
        let loc = Loc::new(doc, PLoc::Name(ANameKey::TyParam(decl_id)));
        ty_resolver
            .env
            .insert_ty(name.to_string(), KTy::Var(KTyVar { name, loc }));
    }
}

fn alloc_const(
    decl_id: ADeclId,
    decl: &AFieldLikeDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
) -> KConst {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    let k_const = mod_outline.consts.alloc(KConstOutline {
        name,
        value_ty: KTy::init_later(Loc::new(doc, PLoc::Decl(decl_id))),
        value_opt: None,
        parent_opt: None,
        loc: Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id))),
    });
    k_const
}

fn resolve_const_decl(
    const_decl: &AFieldLikeDecl,
    k_const: KConst,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let value_ty = resolve_ty_opt(const_decl.ty_opt, ty_resolver);
    k_const.of_mut(&mut mod_outline.consts).value_ty = value_ty;
}

fn alloc_static(
    decl_id: ADeclId,
    decl: &AFieldLikeDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
) -> KStaticVar {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.static_vars.alloc(KStaticVarOutline {
        name,
        ty: KTy::init_later(Loc::new(doc, PLoc::Decl(decl_id))),
        value_opt: None,
        loc: Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id))),
    })
}

fn resolve_static_decl(
    static_decl: &AFieldLikeDecl,
    static_var: KStaticVar,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let value_ty = resolve_ty_opt(static_decl.ty_opt, ty_resolver);
    static_var.of_mut(&mut mod_outline.static_vars).ty = value_ty;
}

fn convert_ty_params(ty_params: &[ATyParamDecl], doc: Doc, decl_id: ADeclId) -> Vec<KTyParam> {
    ty_params
        .iter()
        .map(|ty_param| {
            let name = resolve_name_opt(Some(&ty_param.name));
            KTyParam {
                name,
                loc: Loc::new(doc, PLoc::Name(ANameKey::TyParam(decl_id))),
            }
        })
        .collect()
}

fn resolve_param_tys(param_decls: &[AParamDecl], ty_resolver: &mut TyResolver) -> Vec<KTy> {
    param_decls
        .iter()
        .map(|param_decl| resolve_ty_opt(param_decl.ty_opt, ty_resolver))
        .collect()
}

fn alloc_fn(decl_id: ADeclId, decl: &AFnLikeDecl, doc: Doc, mod_outline: &mut KModOutline) -> KFn {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)));

    let vis_opt = resolve_modifiers(&decl.modifiers);
    let name = resolve_name_opt(decl.name_opt.as_ref());
    let ty_params = convert_ty_params(&decl.ty_params, doc, decl_id);

    mod_outline.fns.alloc(KFnOutline {
        name,
        vis_opt,
        ty_params,
        param_tys: vec![],
        result_ty: KTy::init_later(loc),
        loc,
    })
}

fn alloc_extern_fn(
    decl_id: ADeclId,
    decl: &AFnLikeDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
) -> KExternFn {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)));
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.extern_fns.alloc(KExternFnOutline {
        name,
        param_tys: vec![],
        result_ty: KTy::init_later(loc),
        loc,
    })
}

fn new_const_outline_from_variant(
    decl: &AFieldLikeDecl,
    parent_opt: Option<KConstEnum>,
    doc: Doc,
    key: AVariantDeclKey,
) -> KConstOutline {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Variant(key)));
    let name = resolve_name_opt(decl.name_opt.as_ref());

    KConstOutline {
        name,
        value_ty: KTy::init_later(loc),
        value_opt: None,
        parent_opt,
        loc,
    }
}

fn new_field_loc(doc: Doc, parent: AVariantDeclKey, index: usize) -> Loc {
    Loc::new(doc, PLoc::FieldDecl(AFieldDeclKey::new(parent, index)))
}

fn resolve_field_decl(decl: &AFieldLikeDecl, loc: Loc) -> KFieldOutline {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    KFieldOutline {
        name,
        ty: KTy::init_later(loc),
        loc,
    }
}

fn alloc_unit_like_variant(
    decl: &AFieldLikeDecl,
    parent: KStructParent,
    doc: Doc,
    key: AVariantDeclKey,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KStruct {
    if let Some(ty) = decl.ty_opt {
        logger.error(PLoc::Ty(ty), "この enum 宣言には定数でないバリアントが含まれるので、バリアントに型注釈を書くことはできません。");
    } else if let Some(init) = decl.value_opt {
        logger.error(PLoc::Expr(init), "この enum 宣言には定数でないバリアントが含まれるので、バリアントに初期化式を書くことはできません。");
    }

    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.structs.alloc(KStructOutline {
        name,
        fields: vec![],
        parent,
        loc: new_struct_loc(doc, key),
    })
}

fn alloc_record_variant(
    decl: &ARecordVariantDecl,
    parent: KStructParent,
    doc: Doc,
    key: AVariantDeclKey,
    mod_outline: &mut KModOutline,
) -> KStruct {
    let name = decl.name.text.to_string();

    let fields = {
        let fields = decl.fields.iter().enumerate().map(|(index, field_decl)| {
            resolve_field_decl(field_decl, new_field_loc(doc, key, index))
        });
        mod_outline.fields.alloc_slice(fields).iter().collect()
    };

    mod_outline.structs.alloc(KStructOutline {
        name,
        fields,
        parent,
        loc: new_struct_loc(doc, key),
    })
}

fn resolve_record_variant_decl(
    decl: &ARecordVariantDecl,
    k_struct: KStruct,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let fields = k_struct.fields(&mod_outline.structs).to_owned();
    for (field_decl, field) in decl.fields.iter().zip(fields) {
        let ty = resolve_ty_opt(field_decl.ty_opt, ty_resolver);
        field.of_mut(&mut mod_outline.fields).ty = ty;
    }
}

fn alloc_variant(
    variant_decl: &AVariantDecl,
    parent: KStructParent,
    doc: Doc,
    key: AVariantDeclKey,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KStruct {
    match variant_decl {
        AVariantDecl::Const(decl) => {
            alloc_unit_like_variant(decl, parent, doc, key, mod_outline, logger)
        }
        AVariantDecl::Record(decl) => alloc_record_variant(decl, parent, doc, key, mod_outline),
    }
}

fn resolve_variant_decl(
    decl_id: ADeclId,
    variant_decl: &AVariantDecl,
    k_struct: KStruct,
    doc: Doc,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    match variant_decl {
        AVariantDecl::Const(_) => {}
        AVariantDecl::Record(decl) => {
            ty_resolver.env.enter_scope();
            add_ty_params_to_env(&decl.ty_params, doc, decl_id, ty_resolver);
            resolve_record_variant_decl(decl, k_struct, ty_resolver, mod_outline);
            ty_resolver.env.leave_scope();
        }
    }
}

enum KEnumLike {
    StructEnum(KStructEnum),
    ConstEnum(KConstEnum),
}

fn alloc_enum(
    decl_id: ADeclId,
    decl: &AEnumDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KEnumLike {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)));
    let name = resolve_name_opt(decl.name_opt.as_ref());

    if decl.variants.is_empty() {
        logger.error(
            PLoc::Decl(decl_id),
            "enum には少なくとも1つのバリアントが必要です。",
        );
    }

    if let Some(variants) = decl.as_const_enum() {
        let const_enum = mod_outline.const_enums.alloc(KConstEnumOutline {
            name,
            loc,

            // FIXME: どう決定する?
            repr_ty: KTy::USIZE,

            // 後で設定する。
            variants: Default::default(),
        });

        let variants = mod_outline
            .consts
            .alloc_slice(variants.iter().enumerate().map(|(index, variant_decl)| {
                let key = AVariantDeclKey::Enum(decl_id, index);
                new_const_outline_from_variant(variant_decl, Some(const_enum), doc, key)
            }));

        const_enum.of_mut(&mut mod_outline.const_enums).variants = variants;
        return KEnumLike::ConstEnum(const_enum);
    }

    let struct_enum = mod_outline.struct_enums.alloc(KStructEnumOutline {
        name,
        variants: vec![],
        loc,
    });

    let variants = decl
        .variants
        .iter()
        .enumerate()
        .map(|(index, variant)| {
            let key = AVariantDeclKey::Enum(decl_id, index);
            let parent = KStructParent::new_enum(struct_enum, index);
            alloc_variant(variant, parent, doc, key, mod_outline, logger)
        })
        .collect();

    struct_enum.of_mut(&mut mod_outline.struct_enums).variants = variants;
    KEnumLike::StructEnum(struct_enum)
}

fn resolve_const_enum_decl(
    decl: &AEnumDecl,
    const_enum: KConstEnum,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = const_enum.variants(&mod_outline.const_enums).to_owned();
    for (decl, k_const) in decl.variants.iter().zip(variants.iter()) {
        let decl = decl.as_const().unwrap();
        let value_ty = resolve_ty_opt(decl.ty_opt, ty_resolver);
        k_const.of_mut(&mut mod_outline.consts).value_ty = value_ty;
    }
}

fn resolve_struct_enum_decl(
    decl_id: ADeclId,
    decl: &AEnumDecl,
    struct_enum: KStructEnum,
    doc: Doc,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = struct_enum.variants(&mod_outline.struct_enums).to_owned();
    for (variant_decl, k_struct) in decl.variants.iter().zip(variants) {
        resolve_variant_decl(
            decl_id,
            variant_decl,
            k_struct,
            doc,
            ty_resolver,
            mod_outline,
        );
    }
}

fn new_struct_loc(doc: Doc, key: AVariantDeclKey) -> Loc {
    Loc::new(doc, PLoc::Name(ANameKey::Variant(key)))
}

fn alloc_struct(
    decl_id: ADeclId,
    decl: &AStructDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> Option<KStruct> {
    let key = AVariantDeclKey::Struct(decl_id);
    let ty_params = decl
        .variant_opt
        .as_ref()
        .map(|variant_decl| match variant_decl {
            AVariantDecl::Const(_) => vec![],
            AVariantDecl::Record(decl) => convert_ty_params(&decl.ty_params, doc, decl_id),
        })
        .unwrap_or_default();
    let k_struct = alloc_variant(
        decl.variant_opt.as_ref()?,
        KStructParent::Struct { ty_params },
        doc,
        key,
        mod_outline,
        logger,
    );
    Some(k_struct)
}

fn alloc_alias(
    decl: &AUseDecl,
    loc: Loc,
    tokens: &PTokens,
    mod_outline: &mut KModOutline,
) -> KAlias {
    let (name, path) = match &decl.name_opt {
        Some(AName { quals, text, .. }) => (
            text.to_string(),
            quals
                .iter()
                .map(|token| token.text(tokens))
                .chain(once(text.as_str()))
                .map(|text| text.to_string())
                .collect(),
        ),
        None => Default::default(),
    };

    mod_outline
        .aliases
        .alloc(KAliasOutline::new(name, path, loc))
}

fn alloc_outline(
    doc: Doc,
    tree: &PTree,
    decl_symbols: &mut DeclSymbols,
    env: &mut Env,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) {
    let ast = &tree.ast;

    for ((decl_id, decl), decl_symbol_opt) in ast.decls().enumerate().zip(decl_symbols.iter_mut()) {
        let loc = Loc::new(doc, PLoc::Decl(decl_id));
        let symbol = match decl {
            ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                let k_const = alloc_const(decl_id, &const_decl, doc, mod_outline);
                KModSymbol::Const(k_const)
            }
            ADecl::Static(static_decl) => {
                let static_var = alloc_static(decl_id, &static_decl, doc, mod_outline);
                KModSymbol::StaticVar(static_var)
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = alloc_fn(decl_id, fn_decl, doc, mod_outline);
                KModSymbol::Fn(k_fn)
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = alloc_extern_fn(decl_id, extern_fn_decl, doc, mod_outline);
                KModSymbol::ExternFn(extern_fn)
            }
            ADecl::Enum(enum_decl) => {
                match alloc_enum(decl_id, enum_decl, doc, mod_outline, logger) {
                    KEnumLike::ConstEnum(const_enum) => KModSymbol::ConstEnum(const_enum),
                    KEnumLike::StructEnum(struct_enum) => KModSymbol::StructEnum(struct_enum),
                }
            }
            ADecl::Struct(struct_decl) => {
                let k_struct = match alloc_struct(decl_id, struct_decl, doc, mod_outline, logger) {
                    Some(it) => it,
                    None => continue,
                };
                KModSymbol::Struct(k_struct)
            }
            ADecl::Use(use_decl) => {
                let alias = alloc_alias(use_decl, loc, &tree.tokens, mod_outline);
                KModSymbol::Alias(alias)
            }
        };

        *decl_symbol_opt = Some(symbol);

        // FIXME: スコープを無視している
        let name = symbol.name(&mod_outline);
        do_add_ty_symbol_to_local_env(name, symbol, env);
    }
}

fn resolve_outline(
    doc: Doc,
    tree: &PTree,
    env: &mut Env,
    mod_outline: &mut KModOutline,
    decl_symbols: &DeclSymbols,
    listener: &mut dyn NameResolutionListener,
    logger: &DocLogger,
) {
    let ast = &tree.ast;
    let ty_resolver = &mut TyResolver {
        env,
        ast,
        listener,
        logger,
    };

    for ((decl_id, decl), decl_symbol_opt) in ast.decls().enumerate().zip(decl_symbols.iter()) {
        let symbol = match decl_symbol_opt {
            Some(it) => it,
            None => continue,
        };

        match decl {
            ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                let k_const = match symbol {
                    KModSymbol::Const(it) => it,
                    _ => unreachable!(),
                };
                resolve_const_decl(const_decl, *k_const, ty_resolver, mod_outline);
            }
            ADecl::Static(static_decl) => {
                let static_var = match symbol {
                    KModSymbol::StaticVar(it) => it,
                    _ => unreachable!(),
                };
                resolve_static_decl(static_decl, *static_var, ty_resolver, mod_outline);
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = match symbol {
                    KModSymbol::Fn(it) => it,
                    _ => unreachable!(),
                };

                ty_resolver.env.enter_scope();
                add_ty_params_to_env(&fn_decl.ty_params, doc, decl_id, ty_resolver);

                let param_tys = resolve_param_tys(&fn_decl.params, ty_resolver);
                let result_ty = resolve_ty_or_unit(fn_decl.result_ty_opt, ty_resolver);

                let fn_data = k_fn.of_mut(&mut mod_outline.fns);
                fn_data.param_tys = param_tys;
                fn_data.result_ty = result_ty;

                ty_resolver.env.leave_scope();
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = match symbol {
                    KModSymbol::ExternFn(it) => it,
                    _ => unreachable!(),
                };

                let param_tys = resolve_param_tys(&extern_fn_decl.params, ty_resolver);
                let result_ty = resolve_ty_or_unit(extern_fn_decl.result_ty_opt, ty_resolver);

                let extern_fn_data = extern_fn.of_mut(&mut mod_outline.extern_fns);
                extern_fn_data.param_tys = param_tys;
                extern_fn_data.result_ty = result_ty;
            }
            ADecl::Enum(decl) => match symbol {
                KModSymbol::ConstEnum(const_enum) => {
                    resolve_const_enum_decl(decl, *const_enum, ty_resolver, mod_outline)
                }
                KModSymbol::StructEnum(struct_enum) => resolve_struct_enum_decl(
                    decl_id,
                    decl,
                    *struct_enum,
                    doc,
                    ty_resolver,
                    mod_outline,
                ),
                _ => unreachable!(),
            },
            ADecl::Struct(decl) => {
                let variant_decl = match &decl.variant_opt {
                    Some(it) => it,
                    None => continue,
                };
                let k_struct = match *symbol {
                    KModSymbol::Struct(it) => it,
                    _ => continue,
                };
                resolve_variant_decl(
                    decl_id,
                    variant_decl,
                    k_struct,
                    doc,
                    ty_resolver,
                    mod_outline,
                );
            }
            ADecl::Use(_) => {}
        }
    }
}

pub(crate) fn generate_outline(
    doc: Doc,
    tree: &PTree,
    listener: &mut dyn NameResolutionListener,
    logger: &DocLogger,
) -> (KModOutline, DeclSymbols) {
    let mut decl_symbols = tree.ast.decls().slice().map_with_value(None);
    let mut env = Env::new();
    let mut mod_outline = KModOutline::default();

    env.enter_scope();
    alloc_outline(
        doc,
        tree,
        &mut decl_symbols,
        &mut env,
        &mut mod_outline,
        logger,
    );
    resolve_outline(
        doc,
        tree,
        &mut env,
        &mut mod_outline,
        &decl_symbols,
        listener,
        logger,
    );
    env.leave_scope();

    (mod_outline, decl_symbols)
}
