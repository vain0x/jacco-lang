#![allow(unused)]

use crate::{
    cps::{
        KAliasOutline, KConst, KConstData, KEnum, KEnumOutline, KExternFnOutline, KFieldOutline,
        KFnOutline, KModLocalSymbol, KModOutline, KStaticVarData, KStruct, KStructOutline,
        KStructParent, KTy, KVariant, KVis,
    },
    parse::{
        ADecl, ADeclModifiers, ADeclTag, AEnumDecl, AFieldLikeDecl, AFnLikeDecl, AName, AParamDecl,
        ARecordVariantDecl, AStructDecl, ATree, ATyId, AUseDecl, AVariantDecl,
    },
    source::Loc,
    utils::VecArena,
};

pub(crate) struct Xx {
    // scope: Scope,
    outline: KModOutline,
    decls: VecArena<ADeclTag, Option<KModLocalSymbol>>,
}

fn resolve_modifiers(modifiers: &ADeclModifiers) -> Option<KVis> {
    modifiers.vis_opt
}

fn resolve_ty(_ty: ATyId, _mod_outline: &KModOutline) -> KTy {
    // FIXME: 実装
    KTy::Unresolved
}

fn resolve_ty_opt(ty_opt: Option<ATyId>, mod_outline: &KModOutline) -> KTy {
    ty_opt.map_or(KTy::Unresolved, |ty| resolve_ty(ty, mod_outline))
}

fn resolve_name_opt(name_opt: Option<&AName>) -> String {
    name_opt.map_or(String::new(), |name| name.text.to_string())
}

fn alloc_const(decl: &AFieldLikeDecl, mod_outline: &mut KModOutline) -> KModLocalSymbol {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    let k_const = mod_outline.consts.alloc(KConstData {
        name,
        value_ty: KTy::Unresolved,
        value_opt: None,
        parent_opt: None,
        loc: Loc::Unknown("<const>"),
    });
    KModLocalSymbol::Const(k_const)
}

fn alloc_static(decl: &AFieldLikeDecl, mod_outline: &mut KModOutline) -> KModLocalSymbol {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    let static_var = mod_outline.static_vars.alloc(KStaticVarData {
        name,
        ty: KTy::Unresolved,
        value_opt: None,
        loc: Loc::Unknown("<static>"),
    });
    KModLocalSymbol::StaticVar(static_var)
}

fn resolve_param_tys(param_decls: &[AParamDecl], mod_outline: &mut KModOutline) -> Vec<KTy> {
    param_decls
        .iter()
        .map(|param_decl| resolve_ty_opt(param_decl.ty_opt, mod_outline))
        .collect()
}

fn alloc_fn(decl: &AFnLikeDecl, mod_outline: &mut KModOutline) -> KModLocalSymbol {
    let vis_opt = resolve_modifiers(&decl.modifiers);
    let name = resolve_name_opt(decl.name_opt.as_ref());
    let param_tys = vec![KTy::Unresolved; decl.params.len()];

    let k_fn = mod_outline.fns.alloc(KFnOutline {
        name,
        vis_opt,
        param_tys,
        result_ty: KTy::Unresolved,
        loc: Loc::Unknown("<fn>"),
    });
    KModLocalSymbol::Fn(k_fn)
}

fn alloc_extern_fn(decl: &AFnLikeDecl, mod_outline: &mut KModOutline) -> KModLocalSymbol {
    let name = resolve_name_opt(decl.name_opt.as_ref());
    let param_tys = vec![KTy::Unresolved; decl.params.len()];

    let extern_fn = mod_outline.extern_fns.alloc(KExternFnOutline {
        name,
        param_tys,
        result_ty: KTy::Unresolved,
        loc: Loc::Unknown("<extern fn>"),
    });
    KModLocalSymbol::ExternFn(extern_fn)
}

fn alloc_const_variant(
    decl: &AFieldLikeDecl,
    parent_opt: Option<KEnum>,
    mod_outline: &mut KModOutline,
) -> KConst {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    mod_outline.consts.alloc(KConstData {
        name,
        value_ty: KTy::Unresolved,
        value_opt: None,
        parent_opt,
        loc: Loc::Unknown("<const variant>"),
    })
}

fn resolve_field_decl(decl: &AFieldLikeDecl) -> KFieldOutline {
    let name = resolve_name_opt(decl.name_opt.as_ref());

    KFieldOutline {
        name,
        ty: KTy::Unresolved,
        loc: Loc::Unknown("<field>"),
    }
}

fn alloc_record_variant(
    decl: &ARecordVariantDecl,
    parent_opt: Option<KStructParent>,
    mod_outline: &mut KModOutline,
) -> KStruct {
    let name = decl.name.text.to_string();

    let fields = {
        let fields = decl.fields.iter().map(resolve_field_decl);
        mod_outline.fields.alloc_slice(fields).iter().collect()
    };

    mod_outline.structs.alloc(KStructOutline {
        name,
        fields,
        parent_opt,
        loc: Loc::Unknown("<record variant>"),
    })
}

fn alloc_variant(
    variant_decl: &AVariantDecl,
    parent_opt: Option<KEnum>,
    mod_outline: &mut KModOutline,
) -> KVariant {
    match variant_decl {
        AVariantDecl::Const(decl) => {
            KVariant::Const(alloc_const_variant(decl, parent_opt, mod_outline))
        }
        AVariantDecl::Record(decl) => {
            let parent_opt = parent_opt.map(KStructParent::new);
            KVariant::Record(alloc_record_variant(decl, parent_opt, mod_outline))
        }
    }
}

fn alloc_enum(decl: &AEnumDecl, mod_outline: &mut KModOutline) -> KModLocalSymbol {
    let name = resolve_name_opt(decl.name_opt.as_ref());
    let k_enum = mod_outline.enums.alloc(KEnumOutline {
        name,
        variants: vec![],
        loc: Loc::Unknown("<enum>"),
    });

    let variants = decl
        .variants
        .iter()
        .map(|variant| alloc_variant(variant, Some(k_enum), mod_outline))
        .collect();

    k_enum.of_mut(&mut mod_outline.enums).variants = variants;
    KModLocalSymbol::Enum(k_enum)
}

fn alloc_struct(decl: &AStructDecl, mod_outline: &mut KModOutline) -> Option<KModLocalSymbol> {
    let variant = alloc_variant(decl.variant_opt.as_ref()?, None, mod_outline);

    let symbol = match variant {
        KVariant::Const(k_const) => KModLocalSymbol::Const(k_const),
        KVariant::Record(k_struct) => KModLocalSymbol::Struct(k_struct),
    };
    Some(symbol)
}

fn alloc_alias(decl: &AUseDecl, mod_outline: &mut KModOutline) -> KModLocalSymbol {
    let (name, path) = match &decl.name_opt {
        Some(AName { text, full_name }) => (
            text.to_string(),
            full_name.split("::").map(|part| part.to_string()).collect(),
        ),
        None => Default::default(),
    };

    let alias = mod_outline
        .aliases
        .alloc(KAliasOutline::new(name, path, Loc::Unknown("<alias>")));
    KModLocalSymbol::Alias(alias)
}

pub(crate) fn resolve_outline(ast: &ATree, xx: &mut Xx) {
    for (decl_id, decl) in ast.decls().enumerate() {
        let symbol = match decl {
            ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => alloc_const(&const_decl, &mut xx.outline),
            ADecl::Static(static_decl) => alloc_static(&static_decl, &mut xx.outline),
            ADecl::Fn(fn_decl) => alloc_fn(fn_decl, &mut xx.outline),
            ADecl::ExternFn(extern_fn_decl) => alloc_extern_fn(extern_fn_decl, &mut xx.outline),
            ADecl::Enum(enum_decl) => alloc_enum(enum_decl, &mut xx.outline),
            ADecl::Struct(struct_decl) => match alloc_struct(struct_decl, &mut xx.outline) {
                Some(symbol) => symbol,
                None => continue,
            },
            ADecl::Use(use_decl) => alloc_alias(use_decl, &mut xx.outline),
        };

        let old = decl_id.of_mut(&mut xx.decls).replace(symbol);
        assert_eq!(old, None);
    }
}
