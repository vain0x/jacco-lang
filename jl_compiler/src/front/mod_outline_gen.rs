use super::{
    cps_conversion::{convert_ty, convert_ty_opt, TyResolver},
    name_resolution::*,
};
use crate::{
    cps::*,
    logs::DocLogger,
    parse::*,
    scope::lexical_referent::LexicalReferent,
    source::{Doc, Loc},
};
use std::{collections::HashMap, iter::once};

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

struct OutlineGenerator<'a> {
    #[allow(unused)]
    doc: Doc,
    tokens: &'a PTokens,
    ast: &'a ATree,
    name_symbols: &'a mut NameSymbols,
    mod_outline: &'a mut KModOutline,
}

impl<'a> OutlineGenerator<'a> {
    fn bind_symbol(&mut self, name: ANameId, symbol: KModSymbol) {
        self.name_symbols
            .insert(name, NameSymbol::ModSymbol(symbol));
    }

    fn add_alias(&mut self, decl: &AUseDecl, loc: Loc) {
        let name = match decl.name_opt {
            Some(it) => it,
            None => return,
        };

        let AName { quals, text, .. } = name.of(self.ast.names());
        let text = text.to_string();
        let path = quals
            .iter()
            .map(|token| token.text(self.tokens))
            .chain(once(text.as_str()))
            .map(|text| text.to_string())
            .collect();

        let k_alias = self
            .mod_outline
            .aliases
            .alloc(KAliasOutline::new(text, path, loc));

        self.bind_symbol(name, KModSymbol::Alias(k_alias));
    }

    fn add_const(&mut self, decl: &AFieldLikeDecl, loc: Loc) {
        let name = match decl.name_opt {
            Some(it) => it,
            None => return,
        };

        let k_const = self.mod_outline.consts.alloc(KConstOutline {
            name: name.of(self.ast.names()).text().to_string(),
            value_ty: KTy::init_later(loc),
            value_opt: None,
            parent_opt: None,
            loc: Loc::new(self.doc, PLoc::Name(ANameKey::Id(name))),
        });

        self.bind_symbol(name, KModSymbol::Const(k_const));
    }
}

fn alloc_alias(
    decl: &AUseDecl,
    loc: Loc,
    tokens: &PTokens,
    ast: &ATree,
    name_symbols: &mut NameSymbols,
    mod_outline: &mut KModOutline,
) {
    OutlineGenerator {
        doc: loc.doc().unwrap(),
        tokens,
        ast,
        name_symbols,
        mod_outline,
    }
    .add_alias(decl, loc);
}

fn alloc_const(
    decl: &AFieldLikeDecl,
    loc: Loc,
    tokens: &PTokens,
    ast: &ATree,
    name_symbols: &mut NameSymbols,
    mod_outline: &mut KModOutline,
) {
    OutlineGenerator {
        doc: loc.doc().unwrap(),
        tokens,
        ast,
        name_symbols,
        mod_outline,
    }
    .add_const(decl, loc);
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
    ast: &ATree,
    mod_outline: &mut KModOutline,
) -> KStaticVar {
    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));

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

fn convert_ty_params(
    ty_params: &[ATyParamDecl],
    doc: Doc,
    decl_id: ADeclId,
    ast: &ATree,
    name_symbols: &mut NameSymbols,
) -> Vec<KTyParam> {
    ty_params
        .iter()
        .map(|ty_param| {
            let name_id = ty_param.name;

            let name = resolve_name_opt(Some(ty_param.name.of(ast.names())));
            let ty_param = KTyParam {
                name,
                loc: Loc::new(doc, PLoc::Name(ANameKey::TyParam(decl_id))),
            };

            name_symbols.insert(name_id, NameSymbol::TyParam(ty_param.clone()));
            ty_param
        })
        .collect()
}

fn resolve_param_tys(param_decls: &[AParamDecl], ty_resolver: &mut TyResolver) -> Vec<KTy> {
    param_decls
        .iter()
        .map(|param_decl| resolve_ty_opt(param_decl.ty_opt, ty_resolver))
        .collect()
}

fn alloc_fn(
    decl_id: ADeclId,
    decl: &AFnLikeDecl,
    doc: Doc,
    ast: &ATree,
    name_symbols: &mut NameSymbols,
    mod_outline: &mut KModOutline,
) -> KFn {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)));

    let vis_opt = resolve_modifiers(&decl.modifiers);
    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));
    let ty_params = convert_ty_params(&decl.ty_params, doc, decl_id, ast, name_symbols);

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
    ast: &ATree,
    mod_outline: &mut KModOutline,
) -> KExternFn {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)));
    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));

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
    ast: &ATree,
) -> KConstOutline {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Variant(key)));
    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));

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

fn resolve_field_decl(decl: &AFieldLikeDecl, loc: Loc, ast: &ATree) -> KFieldOutline {
    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));

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
    ast: &ATree,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KStruct {
    if let Some(ty) = decl.ty_opt {
        logger.error(PLoc::Ty(ty), "この enum 宣言には定数でないバリアントが含まれるので、バリアントに型注釈を書くことはできません。");
    } else if let Some(init) = decl.value_opt {
        logger.error(PLoc::Expr(init), "この enum 宣言には定数でないバリアントが含まれるので、バリアントに初期化式を書くことはできません。");
    }

    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));

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
    ast: &ATree,
    mod_outline: &mut KModOutline,
) -> KStruct {
    let name = decl.name.of(ast.names()).text.to_string();

    let fields = {
        let fields = decl.fields.iter().enumerate().map(|(index, field_decl)| {
            resolve_field_decl(field_decl, new_field_loc(doc, key, index), ast)
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
    ast: &ATree,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KStruct {
    match variant_decl {
        AVariantDecl::Const(decl) => {
            alloc_unit_like_variant(decl, parent, doc, key, ast, mod_outline, logger)
        }
        AVariantDecl::Record(decl) => {
            alloc_record_variant(decl, parent, doc, key, ast, mod_outline)
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
    StructEnum(KStructEnum),
    ConstEnum(KConstEnum),
}

fn alloc_enum(
    decl_id: ADeclId,
    decl: &AEnumDecl,
    doc: Doc,
    ast: &ATree,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> KEnumLike {
    let loc = Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)));
    let name = resolve_name_opt(decl.name_opt.map(|name| name.of(ast.names())));

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
                new_const_outline_from_variant(variant_decl, Some(const_enum), doc, key, ast)
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
            alloc_variant(variant, parent, doc, key, ast, mod_outline, logger)
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
    decl: &AEnumDecl,
    struct_enum: KStructEnum,
    ty_resolver: &mut TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = struct_enum.variants(&mod_outline.struct_enums).to_owned();
    for (variant_decl, k_struct) in decl.variants.iter().zip(variants) {
        resolve_variant_decl(variant_decl, k_struct, ty_resolver, mod_outline);
    }
}

fn new_struct_loc(doc: Doc, key: AVariantDeclKey) -> Loc {
    Loc::new(doc, PLoc::Name(ANameKey::Variant(key)))
}

fn alloc_struct(
    decl_id: ADeclId,
    decl: &AStructDecl,
    doc: Doc,
    ast: &ATree,
    name_symbols: &mut NameSymbols,
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> Option<KStruct> {
    let key = AVariantDeclKey::Struct(decl_id);
    let ty_params = decl
        .variant_opt
        .as_ref()
        .map(|variant_decl| match variant_decl {
            AVariantDecl::Const(_) => vec![],
            AVariantDecl::Record(decl) => {
                convert_ty_params(&decl.ty_params, doc, decl_id, ast, name_symbols)
            }
        })
        .unwrap_or_default();
    let k_struct = alloc_variant(
        decl.variant_opt.as_ref()?,
        KStructParent::Struct { ty_params },
        doc,
        key,
        ast,
        mod_outline,
        logger,
    );
    Some(k_struct)
}

fn alloc_outline(
    doc: Doc,
    tree: &PTree,
    mod_outline: &mut KModOutline,
    name_symbols: &mut NameSymbols,
    logger: &DocLogger,
) {
    let ast = &tree.ast;
    let name_referents = &tree.name_referents;

    for (decl_id, decl) in ast.decls().enumerate() {
        let loc = Loc::new(doc, PLoc::Decl(decl_id));
        let symbol = match decl {
            ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                alloc_const(
                    &const_decl,
                    loc,
                    &tree.tokens,
                    ast,
                    name_symbols,
                    mod_outline,
                );
                continue;
            }
            ADecl::Static(static_decl) => {
                let static_var = alloc_static(decl_id, &static_decl, doc, ast, mod_outline);
                KModSymbol::StaticVar(static_var)
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = alloc_fn(decl_id, fn_decl, doc, ast, name_symbols, mod_outline);
                KModSymbol::Fn(k_fn)
            }
            ADecl::ExternFn(extern_fn_decl) => {
                let extern_fn = alloc_extern_fn(decl_id, extern_fn_decl, doc, ast, mod_outline);
                KModSymbol::ExternFn(extern_fn)
            }
            ADecl::Enum(enum_decl) => {
                match alloc_enum(decl_id, enum_decl, doc, ast, mod_outline, logger) {
                    KEnumLike::ConstEnum(const_enum) => KModSymbol::ConstEnum(const_enum),
                    KEnumLike::StructEnum(struct_enum) => KModSymbol::StructEnum(struct_enum),
                }
            }
            ADecl::Struct(struct_decl) => {
                let k_struct = match alloc_struct(
                    decl_id,
                    struct_decl,
                    doc,
                    ast,
                    name_symbols,
                    mod_outline,
                    logger,
                ) {
                    Some(it) => it,
                    None => continue,
                };
                KModSymbol::Struct(k_struct)
            }
            ADecl::Use(use_decl) => {
                alloc_alias(use_decl, loc, &tree.tokens, ast, name_symbols, mod_outline);
                continue;
            }
        };

        if let Some(name) = decl.name_opt() {
            debug_assert_eq!(name_referents.get(&name), Some(&LexicalReferent::Def));
            name_symbols.insert(name, NameSymbol::ModSymbol(symbol));
        }
    }
}

fn resolve_outline(
    tree: &PTree,
    mod_outline: &mut KModOutline,
    name_referents: &NameReferents,
    name_symbols: &NameSymbols,
    logger: &DocLogger,
) {
    let ast = &tree.ast;
    let ty_resolver = &mut TyResolver {
        ast,
        name_referents,
        name_symbols,
        logger,
    };

    for decl in ast.decls().iter() {
        let symbol = match decl
            .name_opt()
            .and_then(|name| name_symbols.get(&name).cloned())
        {
            Some(it) => it,
            None => continue,
        };

        match decl {
            ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) => continue,
            ADecl::Const(const_decl) => {
                let k_const = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::Const(it)) => it,
                    _ => unreachable!(),
                };
                resolve_const_decl(const_decl, k_const, ty_resolver, mod_outline);
            }
            ADecl::Static(static_decl) => {
                let static_var = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::StaticVar(it)) => it,
                    _ => unreachable!(),
                };
                resolve_static_decl(static_decl, static_var, ty_resolver, mod_outline);
            }
            ADecl::Fn(fn_decl) => {
                let k_fn = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::Fn(it)) => it,
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
                    NameSymbol::ModSymbol(KModSymbol::ExternFn(it)) => it,
                    _ => unreachable!(),
                };

                let param_tys = resolve_param_tys(&extern_fn_decl.params, ty_resolver);
                let result_ty = resolve_ty_or_unit(extern_fn_decl.result_ty_opt, ty_resolver);

                let extern_fn_data = extern_fn.of_mut(&mut mod_outline.extern_fns);
                extern_fn_data.param_tys = param_tys;
                extern_fn_data.result_ty = result_ty;
            }
            ADecl::Enum(decl) => match symbol {
                NameSymbol::ModSymbol(KModSymbol::ConstEnum(const_enum)) => {
                    resolve_const_enum_decl(decl, const_enum, ty_resolver, mod_outline)
                }
                NameSymbol::ModSymbol(KModSymbol::StructEnum(struct_enum)) => {
                    resolve_struct_enum_decl(decl, struct_enum, ty_resolver, mod_outline)
                }
                _ => unreachable!(),
            },
            ADecl::Struct(decl) => {
                let variant_decl = match &decl.variant_opt {
                    Some(it) => it,
                    None => continue,
                };
                let k_struct = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::Struct(it)) => it,
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
    logger: &DocLogger,
) -> (KModOutline, NameSymbols) {
    let mut mod_outline = KModOutline::default();
    let mut name_symbols = HashMap::new();

    alloc_outline(doc, tree, &mut mod_outline, &mut name_symbols, logger);
    resolve_outline(
        tree,
        &mut mod_outline,
        &tree.name_referents,
        &name_symbols,
        logger,
    );

    (mod_outline, name_symbols)
}
