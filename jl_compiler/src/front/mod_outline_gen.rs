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

fn alloc_const(
    decl_id: ADeclId,
    decl: &AFieldLikeDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
) -> KConst {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    let k_const = mod_outline.consts.alloc(KConstData {
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

    mod_outline.static_vars.alloc(KStaticVarData {
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

    mod_outline.fns.alloc(KFnOutline {
        name,
        vis_opt,
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

fn new_const_data_from_variant(
    decl: &AFieldLikeDecl,
    parent_opt: Option<KConstParent>,
    doc: Doc,
    key: AVariantDeclKey,
) -> KConstData {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Variant(key)));
    let name = resolve_name_opt(decl.name_opt.as_ref());

    KConstData {
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
    parent_opt: Option<KStructParent>,
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
        parent_opt,
        loc: new_struct_loc(doc, key),
    })
}

fn alloc_record_variant(
    decl: &ARecordVariantDecl,
    parent_opt: Option<KStructParent>,
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
        parent_opt,
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

#[allow(unused)]
fn new_variant_loc(doc: Doc, decl_id: ADeclId, index: usize) -> Loc {
    Loc::new(
        doc,
        PLoc::VariantDecl(AVariantDeclKey::Enum(decl_id, index)),
    )
}

fn alloc_variant(
    variant_decl: &AVariantDecl,
    parent_opt: Option<KEnum>,
    doc: Doc,
    key: AVariantDeclKey,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KStruct {
    match variant_decl {
        AVariantDecl::Const(decl) => {
            let parent_opt = parent_opt.map(KStructParent::new);
            alloc_unit_like_variant(decl, parent_opt, doc, key, mod_outline, logger)
        }
        AVariantDecl::Record(decl) => {
            let parent_opt = parent_opt.map(KStructParent::new);
            alloc_record_variant(decl, parent_opt, doc, key, mod_outline)
        }
    }
}

fn resolve_variant_decl(
    variant_decl: &AVariantDecl,
    k_struct: KStruct,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    match variant_decl {
        AVariantDecl::Const(_) => {}
        AVariantDecl::Record(decl) => {
            resolve_record_variant_decl(decl, k_struct, ty_resolver, mod_outline);
        }
    }
}

enum KEnumLike {
    Enum(KEnum),
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
                let parent_opt = Some(KConstParent::ConstEnum(const_enum));
                new_const_data_from_variant(variant_decl, parent_opt, doc, key)
            }));

        const_enum.of_mut(&mut mod_outline.const_enums).variants = variants;
        return KEnumLike::ConstEnum(const_enum);
    }

    let k_enum = mod_outline.enums.alloc(KEnumOutline {
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
            let k_struct = alloc_variant(variant, Some(k_enum), doc, key, mod_outline, logger);
            KVariant::Record(k_struct)
        })
        .collect();

    k_enum.of_mut(&mut mod_outline.enums).variants = variants;
    KEnumLike::Enum(k_enum)
}

fn resolve_enum_decl(
    decl: &AEnumDecl,
    k_enum: KEnum,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = k_enum.variants(&mod_outline.enums).to_owned();
    for (variant_decl, variant) in decl.variants.iter().zip(variants) {
        let k_struct = variant.as_record().unwrap();
        resolve_variant_decl(variant_decl, k_struct, ty_resolver, mod_outline);
    }
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

fn new_struct_loc(doc: Doc, key: AVariantDeclKey) -> Loc {
    Loc::new(doc, PLoc::Name(ANameKey::Variant(key)))
}

fn alloc_struct(
    decl_id: ADeclId,
    decl: &AStructDecl,
    doc: Doc,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> Option<KVariant> {
    let key = AVariantDeclKey::Struct(decl_id);
    let k_struct = alloc_variant(
        decl.variant_opt.as_ref()?,
        None,
        doc,
        key,
        mod_outline,
        logger,
    );
    Some(KVariant::Record(k_struct))
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
                KModLocalSymbol::Const(k_const)
            }
            ADecl::Static(static_decl) => {
                let static_var = alloc_static(decl_id, &static_decl, doc, mod_outline);
                KModLocalSymbol::StaticVar(static_var)
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = alloc_fn(decl_id, fn_decl, doc, mod_outline);
                KModLocalSymbol::Fn(k_fn)
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = alloc_extern_fn(decl_id, extern_fn_decl, doc, mod_outline);
                KModLocalSymbol::ExternFn(extern_fn)
            }
            ADecl::Enum(enum_decl) => {
                match alloc_enum(decl_id, enum_decl, doc, mod_outline, logger) {
                    KEnumLike::Enum(k_enum) => KModLocalSymbol::Enum(k_enum),
                    KEnumLike::ConstEnum(const_enum) => KModLocalSymbol::ConstEnum(const_enum),
                }
            }
            ADecl::Struct(struct_decl) => {
                let variant = match alloc_struct(decl_id, struct_decl, doc, mod_outline, logger) {
                    Some(it) => it,
                    None => continue,
                };
                KModLocalSymbol::from_variant(variant)
            }
            ADecl::Use(use_decl) => {
                let alias = alloc_alias(use_decl, loc, &tree.tokens, mod_outline);
                KModLocalSymbol::Alias(alias)
            }
        };

        *decl_symbol_opt = Some(symbol);

        // FIXME: スコープを無視している
        // FIXME: ローカル変数でなければ名前を取れるので unwrap は成功するが、unwrap は使うべきでない
        let name = symbol.name(&mod_outline).unwrap();
        do_add_ty_symbol_to_local_env(name, symbol, env);
    }
}

fn resolve_outline(
    tree: &PTree,
    env: &Env,
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

    for (decl, decl_symbol_opt) in ast.decls().iter().zip(decl_symbols.iter()) {
        let symbol = match decl_symbol_opt {
            Some(it) => it,
            None => continue,
        };

        match decl {
            ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                let k_const = match symbol {
                    KModLocalSymbol::Const(it) => it,
                    _ => unreachable!(),
                };
                resolve_const_decl(const_decl, *k_const, ty_resolver, mod_outline);
            }
            ADecl::Static(static_decl) => {
                let static_var = match symbol {
                    KModLocalSymbol::StaticVar(it) => it,
                    _ => unreachable!(),
                };
                resolve_static_decl(static_decl, *static_var, ty_resolver, mod_outline);
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = match symbol {
                    KModLocalSymbol::Fn(it) => it,
                    _ => unreachable!(),
                };

                let param_tys = resolve_param_tys(&fn_decl.params, ty_resolver);
                let result_ty = resolve_ty_or_unit(fn_decl.result_ty_opt, ty_resolver);

                let fn_data = k_fn.of_mut(&mut mod_outline.fns);
                fn_data.param_tys = param_tys;
                fn_data.result_ty = result_ty;
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = match symbol {
                    KModLocalSymbol::ExternFn(it) => it,
                    _ => unreachable!(),
                };

                let param_tys = resolve_param_tys(&extern_fn_decl.params, ty_resolver);
                let result_ty = resolve_ty_or_unit(extern_fn_decl.result_ty_opt, ty_resolver);

                let extern_fn_data = extern_fn.of_mut(&mut mod_outline.extern_fns);
                extern_fn_data.param_tys = param_tys;
                extern_fn_data.result_ty = result_ty;
            }
            ADecl::Enum(decl) => match symbol {
                KModLocalSymbol::Enum(k_enum) => {
                    resolve_enum_decl(decl, *k_enum, ty_resolver, mod_outline)
                }
                KModLocalSymbol::ConstEnum(const_enum) => {
                    resolve_const_enum_decl(decl, *const_enum, ty_resolver, mod_outline)
                }
                _ => unreachable!(),
            },
            ADecl::Struct(decl) => {
                let variant_decl = match &decl.variant_opt {
                    Some(it) => it,
                    None => continue,
                };
                let k_struct = match *symbol {
                    KModLocalSymbol::Struct(it) => it,
                    _ => continue,
                };
                resolve_variant_decl(variant_decl, k_struct, ty_resolver, mod_outline);
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
        tree,
        &env,
        &mut mod_outline,
        &decl_symbols,
        listener,
        logger,
    );
    env.leave_scope();

    (mod_outline, decl_symbols)
}
