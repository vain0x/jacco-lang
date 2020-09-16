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
    doc: Doc,
    tokens: &'a PTokens,
    ast: &'a ATree,
    name_referents: &'a NameReferents,
    name_symbols: &'a mut NameSymbols,
    mod_outline: &'a mut KModOutline,
    logger: &'a DocLogger,
}

impl<'a> OutlineGenerator<'a> {
    fn do_bind_symbol(&mut self, name: ANameId, symbol: NameSymbol) {
        assert_eq!(self.name_referents[&name], LexicalReferent::Def);

        let old = self.name_symbols.insert(name, symbol);
        assert!(old.is_none());
    }

    fn bind_symbol(&mut self, name: ANameId, symbol: KModSymbol) {
        self.do_bind_symbol(name, NameSymbol::ModSymbol(symbol));
    }

    fn on_ty_params(&mut self, ty_params: &[ATyParamDecl]) -> Vec<KTyParam> {
        ty_params
            .iter()
            .map(|ty_param| {
                let name = ty_param.name;
                let ty_param = KTyParam {
                    name: name.of(self.ast.names()).text().to_string(),
                    loc: name.loc().to_loc(self.doc),
                };

                self.do_bind_symbol(name, NameSymbol::TyParam(ty_param.clone()));
                ty_param
            })
            .collect()
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
            loc: name.loc().to_loc(self.doc),
        });

        self.bind_symbol(name, KModSymbol::Const(k_const));
    }

    fn add_static_var(&mut self, decl: &AFieldLikeDecl, loc: Loc) {
        let name = match decl.name_opt {
            Some(it) => it,
            None => return,
        };

        let static_var = self.mod_outline.static_vars.alloc(KStaticVarOutline {
            name: name.of(self.ast.names()).text().to_string(),
            ty: KTy::init_later(loc),
            value_opt: None,
            loc: name.loc().to_loc(self.doc),
        });

        self.bind_symbol(name, KModSymbol::StaticVar(static_var));
    }

    fn add_fn(&mut self, decl: &AFnLikeDecl, loc: Loc) {
        let name = match decl.name_opt {
            Some(it) => it,
            None => return,
        };

        let vis_opt = resolve_modifiers(&decl.modifiers);
        let ty_params = self.on_ty_params(&decl.ty_params);

        let k_fn = self.mod_outline.fns.alloc(KFnOutline {
            name: name.of(self.ast.names()).text().to_string(),
            vis_opt,
            ty_params,
            param_tys: vec![],
            result_ty: KTy::init_later(loc),
            loc: name.loc().to_loc(self.doc),
        });

        self.bind_symbol(name, KModSymbol::Fn(k_fn));
    }

    fn add_extern_fn(&mut self, decl: &AFnLikeDecl, loc: Loc) {
        let name = match decl.name_opt {
            Some(it) => it,
            None => return,
        };

        let extern_fn = self.mod_outline.extern_fns.alloc(KExternFnOutline {
            name: name.of(self.ast.names()).text().to_string(),
            param_tys: vec![],
            result_ty: KTy::init_later(loc),
            loc: name.loc().to_loc(self.doc),
        });

        self.bind_symbol(name, KModSymbol::ExternFn(extern_fn));
    }

    fn add_const_enum_variant(
        &mut self,
        const_enum: KConstEnum,
        variant_decls: &[&AFieldLikeDecl],
        loc: Loc,
    ) -> KConsts {
        let (doc, ast) = (self.doc, self.ast);
        self.mod_outline
            .consts
            .alloc_slice(variant_decls.iter().map(|variant_decl| {
                let name_opt = variant_decl.name_opt;
                let loc = match name_opt {
                    Some(name) => name.loc().to_loc(doc),
                    None => loc,
                };

                KConstOutline {
                    name: resolve_name_opt(name_opt.map(|name| name.of(ast.names()))),
                    value_ty: KTy::init_later(loc),
                    value_opt: None,
                    parent_opt: Some(const_enum),
                    loc,
                }
            }))
    }

    fn add_const_enum(&mut self, decl: &AEnumDecl, variant_decls: &[&AFieldLikeDecl], loc: Loc) {
        let name_opt = decl.name_opt;

        let const_enum = self.mod_outline.const_enums.alloc(KConstEnumOutline {
            name: resolve_name_opt(name_opt.map(|name| name.of(self.ast.names()))),
            loc: match name_opt {
                Some(name) => name.loc().to_loc(self.doc),
                None => loc,
            },

            // FIXME: どう決定する?
            repr_ty: KTy::USIZE,

            // 後で設定する。
            variants: Default::default(),
        });

        let variants = self.add_const_enum_variant(const_enum, variant_decls, loc);
        const_enum
            .of_mut(&mut self.mod_outline.const_enums)
            .variants = variants;

        if let Some(name) = decl.name_opt {
            self.bind_symbol(name, KModSymbol::ConstEnum(const_enum));
        }
    }

    fn add_unit_like_variant(
        &mut self,
        decl: &AFieldLikeDecl,
        parent: KStructParent,
        key: AVariantDeclKey,
    ) -> KStruct {
        if let Some(ty) = decl.ty_opt {
            self.logger.error(PLoc::Ty(ty), "この enum 宣言には定数でないバリアントが含まれるので、バリアントに型注釈を書くことはできません。");
        } else if let Some(init) = decl.value_opt {
            self.logger.error(PLoc::Expr(init), "この enum 宣言には定数でないバリアントが含まれるので、バリアントに初期化式を書くことはできません。");
        }

        self.mod_outline.structs.alloc(KStructOutline {
            name: resolve_name_opt(decl.name_opt.map(|name| name.of(self.ast.names()))),
            fields: vec![],
            parent,
            loc: match decl.name_opt {
                Some(name) => name.loc().to_loc(self.doc),
                None => new_struct_loc(self.doc, key),
            },
        })
    }

    fn add_record_variant(
        &mut self,
        decl: &ARecordVariantDecl,
        parent: KStructParent,
        key: AVariantDeclKey,
    ) -> KStruct {
        let (doc, ast) = (self.doc, self.ast);
        let fields = {
            let fields = decl.fields.iter().enumerate().map(|(index, field_decl)| {
                let name_opt = field_decl.name_opt;
                let loc = new_field_loc(doc, key, index);
                KFieldOutline {
                    name: resolve_name_opt(name_opt.map(|name| name.of(ast.names()))),
                    ty: KTy::init_later(loc),
                    loc,
                }
            });
            self.mod_outline.fields.alloc_slice(fields).iter().collect()
        };

        self.mod_outline.structs.alloc(KStructOutline {
            name: decl.name.of(self.ast.names()).text.to_string(),
            fields,
            parent,
            loc: new_struct_loc(self.doc, key),
        })
    }

    fn add_struct_variant(
        &mut self,
        decl: &AVariantDecl,
        parent: KStructParent,
        key: AVariantDeclKey,
    ) -> KStruct {
        match decl {
            AVariantDecl::Const(decl) => self.add_unit_like_variant(decl, parent, key),
            AVariantDecl::Record(decl) => self.add_record_variant(decl, parent, key),
        }
    }

    fn add_struct_enum(&mut self, decl_id: ADeclId, decl: &AEnumDecl, loc: Loc) {
        let loc = match decl.name_opt {
            Some(name) => name.loc().to_loc(self.doc),
            None => loc,
        };

        let struct_enum = self.mod_outline.struct_enums.alloc(KStructEnumOutline {
            name: resolve_name_opt(decl.name_opt.map(|name| name.of(self.ast.names()))),
            loc,

            // 後で設定する。
            variants: vec![],
        });

        let variants = decl
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| {
                let key = AVariantDeclKey::Enum(decl_id, index);
                let parent = KStructParent::new_enum(struct_enum, index);
                self.add_struct_variant(variant, parent, key)
            })
            .collect();

        struct_enum
            .of_mut(&mut self.mod_outline.struct_enums)
            .variants = variants;

        if let Some(name) = decl.name_opt {
            self.bind_symbol(name, KModSymbol::StructEnum(struct_enum));
        }
    }

    fn add_enum(&mut self, decl_id: ADeclId, decl: &AEnumDecl, loc: PLoc) {
        if decl.variants.is_empty() {
            self.logger
                .error(loc, "enum には少なくとも1つのバリアントが必要です。");
            return;
        }

        if let Some(variants) = decl.as_const_enum() {
            self.add_const_enum(decl, &variants, loc.to_loc(self.doc));
            return;
        }

        self.add_struct_enum(decl_id, decl, loc.to_loc(self.doc));
    }

    fn add_struct(&mut self, decl_id: ADeclId, decl: &AStructDecl) {
        let key = AVariantDeclKey::Struct(decl_id);

        let decl = match &decl.variant_opt {
            Some(it) => it,
            None => return,
        };

        let ty_params = match decl {
            AVariantDecl::Const(_) => vec![],
            AVariantDecl::Record(decl) => self.on_ty_params(&decl.ty_params),
        };

        let k_struct = self.add_struct_variant(decl, KStructParent::Struct { ty_params }, key);

        if let Some(name) = decl.name_opt() {
            self.bind_symbol(name, KModSymbol::Struct(k_struct));
        }
    }

    fn generate_all(&mut self) {
        for (decl_id, decl) in self.ast.decls().enumerate() {
            let loc = Loc::new(self.doc, PLoc::Decl(decl_id));
            match decl {
                ADecl::Attr | ADecl::Expr(_) | ADecl::Let(_) => continue,
                ADecl::Const(decl) => self.add_const(&decl, loc),
                ADecl::Static(decl) => self.add_static_var(&decl, loc),
                ADecl::Fn(decl) => self.add_fn(decl, loc),
                ADecl::ExternFn(decl) => self.add_extern_fn(decl, loc),
                ADecl::Enum(enum_decl) => self.add_enum(decl_id, enum_decl, PLoc::Decl(decl_id)),
                ADecl::Struct(struct_decl) => self.add_struct(decl_id, struct_decl),
                ADecl::Use(use_decl) => self.add_alias(use_decl, loc),
            }
        }
    }
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

fn new_field_loc(doc: Doc, parent: AVariantDeclKey, index: usize) -> Loc {
    Loc::new(doc, PLoc::FieldDecl(AFieldDeclKey::new(parent, index)))
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
    mod_outline: &mut KModOutline,
    logger: &DocLogger,
) -> NameSymbols {
    let mut name_symbols = HashMap::new();

    OutlineGenerator {
        doc,
        tokens: &tree.tokens,
        ast: &tree.ast,
        name_referents: &tree.name_referents,
        name_symbols: &mut name_symbols,
        mod_outline: mod_outline,
        logger,
    }
    .generate_all();

    resolve_outline(
        tree,
        mod_outline,
        &tree.name_referents,
        &name_symbols,
        logger,
    );

    name_symbols
}
