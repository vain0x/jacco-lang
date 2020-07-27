#![allow(unused)]

use super::{
    cps_conversion::{convert_ty_opt, TyResolver},
    env::Env,
    name_resolution::{add_decl_to_local_env, do_add_ty_symbol_to_local_env, DeclSymbols},
};
use crate::{
    cps::*,
    logs::DocLogger,
    parse::*,
    source::{Doc, Loc},
    utils::VecArena,
};

fn resolve_modifiers(modifiers: &ADeclModifiers) -> Option<KVis> {
    modifiers.vis_opt
}

fn resolve_name_opt(name_opt: Option<&AName>) -> String {
    name_opt.map_or(String::new(), |name| name.text.to_string())
}

fn resolve_ty_opt(ty_opt: Option<ATyId>, ty_resolver: &TyResolver) -> KTy {
    convert_ty_opt(ty_opt, ty_resolver)
}

fn alloc_const(decl: &AFieldLikeDecl, loc: Loc, mod_outline: &mut KModOutline) -> KConst {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    let k_const = mod_outline.consts.alloc(KConstData {
        name,
        value_ty: KTy::Unresolved,
        value_opt: None,
        parent_opt: None,
        loc,
    });
    k_const
}

fn resolve_const_decl(
    const_decl: &AFieldLikeDecl,
    k_const: KConst,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let value_ty = resolve_ty_opt(const_decl.ty_opt, ty_resolver);
    k_const.of_mut(&mut mod_outline.consts).value_ty = value_ty;
}

fn alloc_static(decl: &AFieldLikeDecl, loc: Loc, mod_outline: &mut KModOutline) -> KStaticVar {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.static_vars.alloc(KStaticVarData {
        name,
        ty: KTy::Unresolved,
        value_opt: None,
        loc,
    })
}

fn resolve_static_decl(
    static_decl: &AFieldLikeDecl,
    static_var: KStaticVar,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let value_ty = resolve_ty_opt(static_decl.ty_opt, ty_resolver);
    static_var.of_mut(&mut mod_outline.static_vars).ty = value_ty;
}

fn resolve_param_tys(param_decls: &[AParamDecl], ty_resolver: &TyResolver) -> Vec<KTy> {
    param_decls
        .iter()
        .map(|param_decl| resolve_ty_opt(param_decl.ty_opt, ty_resolver))
        .collect()
}

fn alloc_fn(decl: &AFnLikeDecl, loc: Loc, mod_outline: &mut KModOutline) -> KFn {
    let vis_opt = resolve_modifiers(&decl.modifiers);
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.fns.alloc(KFnOutline {
        name,
        vis_opt,
        param_tys: vec![],
        result_ty: KTy::Unresolved,
        loc,
    })
}

fn alloc_extern_fn(decl: &AFnLikeDecl, loc: Loc, mod_outline: &mut KModOutline) -> KExternFn {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.extern_fns.alloc(KExternFnOutline {
        name,
        param_tys: vec![],
        result_ty: KTy::Unresolved,
        loc,
    })
}

fn alloc_const_variant(
    decl: &AFieldLikeDecl,
    parent_opt: Option<KEnum>,
    loc: Loc,
    mod_outline: &mut KModOutline,
) -> KConst {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.consts.alloc(KConstData {
        name,
        value_ty: KTy::Unresolved,
        value_opt: None,
        parent_opt,
        loc,
    })
}

fn resolve_field_decl(decl: &AFieldLikeDecl, loc: Loc) -> KFieldOutline {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    KFieldOutline {
        name,
        ty: KTy::Unresolved,
        loc,
    }
}

fn alloc_record_variant(
    decl: &ARecordVariantDecl,
    parent_opt: Option<KStructParent>,
    loc: Loc,
    mod_outline: &mut KModOutline,
) -> KStruct {
    let name = decl.name.text.to_string();

    let fields = {
        let fields = decl.fields.iter().map(|field_decl| {
            // FIXME: フィールドのロケーションを取る
            resolve_field_decl(field_decl, loc)
        });
        mod_outline.fields.alloc_slice(fields).iter().collect()
    };

    mod_outline.structs.alloc(KStructOutline {
        name,
        fields,
        parent_opt,
        loc,
    })
}

fn alloc_variant(
    variant_decl: &AVariantDecl,
    parent_opt: Option<KEnum>,
    loc: Loc,
    mod_outline: &mut KModOutline,
) -> KVariant {
    match variant_decl {
        AVariantDecl::Const(decl) => {
            KVariant::Const(alloc_const_variant(decl, parent_opt, loc, mod_outline))
        }
        AVariantDecl::Record(decl) => {
            let parent_opt = parent_opt.map(KStructParent::new);
            KVariant::Record(alloc_record_variant(decl, parent_opt, loc, mod_outline))
        }
    }
}

fn resolve_variant_decl(
    variant_decl: &AVariantDecl,
    variant: KVariant,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    match variant_decl {
        AVariantDecl::Const(decl) => {
            let k_const = variant.as_const().unwrap();
            let value_ty = resolve_ty_opt(decl.ty_opt, ty_resolver);
            k_const.of_mut(&mut mod_outline.consts).value_ty = value_ty;
        }
        AVariantDecl::Record(decl) => {
            let k_struct = variant.as_record().unwrap();
            let fields = k_struct.fields(&mod_outline.structs).to_owned();
            for (field_decl, field) in decl.fields.iter().zip(fields) {
                let ty = resolve_ty_opt(field_decl.ty_opt, ty_resolver);
                field.of_mut(&mut mod_outline.fields).ty = ty;
            }
        }
    }
}

fn alloc_enum(decl: &AEnumDecl, loc: Loc, mod_outline: &mut KModOutline) -> KEnum {
    let name = resolve_name_opt(decl.name_opt.as_ref());
    let k_enum = mod_outline.enums.alloc(KEnumOutline {
        name,
        variants: vec![],
        loc,
    });

    let variants = decl
        .variants
        .iter()
        .map(|variant| {
            // FIXME: バリアントのロケーションを取る
            alloc_variant(variant, Some(k_enum), loc, mod_outline)
        })
        .collect();

    k_enum.of_mut(&mut mod_outline.enums).variants = variants;
    k_enum
}

fn resolve_enum_decl(
    decl: &AEnumDecl,
    k_enum: KEnum,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = k_enum.variants(&mod_outline.enums).to_owned();
    for (variant_decl, variant) in decl.variants.iter().zip(variants) {
        resolve_variant_decl(variant_decl, variant, &ty_resolver, mod_outline);
    }
}

fn alloc_struct(decl: &AStructDecl, loc: Loc, mod_outline: &mut KModOutline) -> Option<KVariant> {
    let variant = alloc_variant(decl.variant_opt.as_ref()?, None, loc, mod_outline);
    Some(variant)
}

fn resolve_struct_decl(
    decl: &AStructDecl,
    variant: KVariant,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    if let Some(variant_decl) = &decl.variant_opt {
        resolve_variant_decl(variant_decl, variant, ty_resolver, mod_outline);
    }
}

fn alloc_alias(decl: &AUseDecl, loc: Loc, mod_outline: &mut KModOutline) -> KAlias {
    let (name, path) = match &decl.name_opt {
        Some(AName { text, full_name }) => (
            text.to_string(),
            full_name.split("::").map(|part| part.to_string()).collect(),
        ),
        None => Default::default(),
    };

    mod_outline
        .aliases
        .alloc(KAliasOutline::new(name, path, loc))
}

fn alloc_outline(
    doc: Doc,
    root: &PRoot,
    decl_symbols: &mut DeclSymbols,
    env: &mut Env,
    mod_outline: &mut KModOutline,
) {
    let ast = &root.ast;

    for ((decl_id, decl), decl_symbol_opt) in ast.decls().enumerate().zip(decl_symbols.iter_mut()) {
        let loc = decl_id.loc(root).to_loc(doc);
        let symbol = match decl {
            ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                let k_const = alloc_const(&const_decl, loc, mod_outline);
                KModLocalSymbol::Const(k_const)
            }
            ADecl::Static(static_decl) => {
                let static_var = alloc_static(&static_decl, loc, mod_outline);
                KModLocalSymbol::StaticVar(static_var)
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = alloc_fn(fn_decl, loc, mod_outline);
                KModLocalSymbol::Fn(k_fn)
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = alloc_extern_fn(extern_fn_decl, loc, mod_outline);
                KModLocalSymbol::ExternFn(extern_fn)
            }
            ADecl::Enum(enum_decl) => {
                let k_enum = alloc_enum(enum_decl, loc, mod_outline);
                KModLocalSymbol::Enum(k_enum)
            }
            ADecl::Struct(struct_decl) => {
                let variant = match alloc_struct(struct_decl, loc, mod_outline) {
                    Some(it) => it,
                    None => continue,
                };
                KModLocalSymbol::from_variant(variant)
            }
            ADecl::Use(use_decl) => {
                let alias = alloc_alias(use_decl, loc, mod_outline);
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
    root: &PRoot,
    env: &Env,
    mod_outline: &mut KModOutline,
    decl_symbols: &DeclSymbols,
    logger: &DocLogger,
) {
    let ast = &root.ast;
    let ty_resolver = TyResolver {
        env,
        root,
        ast,
        logger,
    };

    for (decl, decl_symbol_opt) in ast.decls().iter().zip(decl_symbols.iter()) {
        let symbol = match decl_symbol_opt {
            Some(it) => it,
            None => continue,
        };

        match decl {
            ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                let k_const = match symbol {
                    KModLocalSymbol::Const(it) => it,
                    _ => unreachable!(),
                };
                resolve_const_decl(const_decl, *k_const, &ty_resolver, mod_outline);
            }
            ADecl::Static(static_decl) => {
                let static_var = match symbol {
                    KModLocalSymbol::StaticVar(it) => it,
                    _ => unreachable!(),
                };
                resolve_static_decl(static_decl, *static_var, &ty_resolver, mod_outline);
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = match symbol {
                    KModLocalSymbol::Fn(it) => it,
                    _ => unreachable!(),
                };

                let param_tys = resolve_param_tys(&fn_decl.params, &ty_resolver);
                let result_ty = resolve_ty_opt(fn_decl.result_ty_opt, &ty_resolver);

                let fn_data = k_fn.of_mut(&mut mod_outline.fns);
                fn_data.param_tys = param_tys;
                fn_data.result_ty = result_ty;
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = match symbol {
                    KModLocalSymbol::ExternFn(it) => it,
                    _ => unreachable!(),
                };

                let param_tys = resolve_param_tys(&extern_fn_decl.params, &ty_resolver);
                let result_ty = resolve_ty_opt(extern_fn_decl.result_ty_opt, &ty_resolver);

                let extern_fn_data = extern_fn.of_mut(&mut mod_outline.extern_fns);
                extern_fn_data.param_tys = param_tys;
                extern_fn_data.result_ty = result_ty;
            }
            ADecl::Enum(decl) => {
                let k_enum = match symbol {
                    KModLocalSymbol::Enum(it) => it,
                    _ => unreachable!(),
                };
                resolve_enum_decl(decl, *k_enum, &ty_resolver, mod_outline);
            }
            ADecl::Struct(decl) => {
                let variant_decl = match &decl.variant_opt {
                    Some(it) => it,
                    None => continue,
                };
                let variant = symbol.as_variant().unwrap();
                resolve_variant_decl(variant_decl, variant, &ty_resolver, mod_outline);
            }
            ADecl::Use(_) => {}
        }
    }
}

pub(crate) fn generate_outline(doc: Doc, root: &PRoot, logger: &DocLogger) -> KModOutline {
    let mut decl_symbols = root.ast.decls().slice().map_with_value(None);
    let mut env = Env::new();
    let mut mod_outline = KModOutline::default();

    alloc_outline(doc, root, &mut decl_symbols, &mut env, &mut mod_outline);
    resolve_outline(root, &env, &mut mod_outline, &decl_symbols, logger);

    mod_outline
}
