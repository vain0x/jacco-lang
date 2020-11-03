use super::{cps_gen_ty::*, name_resolution::*};
use crate::{
    cps::*,
    logs::DocLogger,
    parse::*,
    scope::lexical_referent::LexicalReferent,
    source::{Doc, Loc},
};
use std::{collections::HashMap, iter::once};

fn resolve_modifiers(modifiers: &AStmtModifiers) -> Option<KVis> {
    modifiers.vis_opt
}

fn resolve_name_opt(name_opt: Option<&AName>) -> String {
    name_opt.map_or(String::new(), |name| name.text.to_string())
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

    fn add_alias(&mut self, stmt: &AUseStmt, loc: Loc) {
        let name = match stmt.name_opt {
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

    fn add_const(&mut self, stmt: &AFieldLikeDecl, loc: Loc) {
        let name = match stmt.name_opt {
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

    fn add_static_var(&mut self, stmt: &AFieldLikeDecl, loc: Loc) {
        let name = match stmt.name_opt {
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

    fn add_fn(&mut self, stmt: &AFnLikeStmt, loc: Loc) {
        let name = match stmt.name_opt {
            Some(it) => it,
            None => return,
        };

        let vis_opt = resolve_modifiers(&stmt.modifiers);
        let ty_params = self.on_ty_params(&stmt.ty_params);

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

    fn add_extern_fn(&mut self, stmt: &AFnLikeStmt, loc: Loc) {
        let name = match stmt.name_opt {
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

    fn add_const_enum(&mut self, stmt: &AEnumStmt, variant_decls: &[&AFieldLikeDecl], loc: Loc) {
        let name_opt = stmt.name_opt;

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

        if let Some(name) = stmt.name_opt {
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
            self.logger.error(PLoc::Ty(ty), "この enum 文には定数でないバリアントが含まれるので、バリアントに型注釈を書くことはできません。");
        } else if let Some(init) = decl.value_opt {
            self.logger.error(PLoc::Expr(init), "この enum 文には定数でないバリアントが含まれるので、バリアントに初期化式を書くことはできません。");
        }

        self.mod_outline.structs.alloc(KStructOutline {
            name: resolve_name_opt(decl.name_opt.map(|name| name.of(self.ast.names()))),
            fields: vec![],
            parent,
            loc: match decl.name_opt {
                Some(name) => name.loc().to_loc(self.doc),
                None => key.loc().to_loc(self.doc),
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
            self.mod_outline
                .fields
                .alloc_slice(fields)
                .iter()
                .collect::<Vec<_>>()
        };

        for (field_decl, &field) in decl.fields.iter().zip(fields.iter()) {
            if let Some(name) = field_decl.name_opt {
                self.bind_symbol(name, KModSymbol::Field(field));
            }
        }

        self.mod_outline.structs.alloc(KStructOutline {
            name: decl.name.of(self.ast.names()).text.to_string(),
            fields,
            parent,
            loc: decl.name.loc().to_loc(self.doc),
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

    fn add_struct_enum(&mut self, stmt_id: AStmtId, stmt: &AEnumStmt, loc: Loc) {
        let loc = match stmt.name_opt {
            Some(name) => name.loc().to_loc(self.doc),
            None => loc,
        };

        let struct_enum = self.mod_outline.struct_enums.alloc(KStructEnumOutline {
            name: resolve_name_opt(stmt.name_opt.map(|name| name.of(self.ast.names()))),
            loc,

            // 後で設定する。
            variants: vec![],
        });

        let variants = stmt
            .variants
            .iter()
            .enumerate()
            .map(|(index, variant)| {
                let key = AVariantDeclKey::Enum(stmt_id, index);
                let parent = KStructParent::new_enum(struct_enum, index);
                self.add_struct_variant(variant, parent, key)
            })
            .collect();

        struct_enum
            .of_mut(&mut self.mod_outline.struct_enums)
            .variants = variants;

        if let Some(name) = stmt.name_opt {
            self.bind_symbol(name, KModSymbol::StructEnum(struct_enum));
        }
    }

    fn add_enum(&mut self, stmt_id: AStmtId, stmt: &AEnumStmt, loc: PLoc) {
        if stmt.variants.is_empty() {
            self.logger
                .error(loc, "enum には少なくとも1つのバリアントが必要です。");
            return;
        }

        if let Some(variants) = stmt.as_const_enum() {
            self.add_const_enum(stmt, &variants, loc.to_loc(self.doc));
            return;
        }

        self.add_struct_enum(stmt_id, stmt, loc.to_loc(self.doc));
    }

    fn add_struct(&mut self, stmt_id: AStmtId, stmt: &AStructStmt) {
        let key = AVariantDeclKey::Struct(stmt_id);

        let decl = match &stmt.variant_opt {
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
        for (stmt_id, stmt) in self.ast.stmts().enumerate() {
            let loc = Loc::new(self.doc, PLoc::Stmt(stmt_id));
            match stmt {
                AStmt::Attr | AStmt::Expr(_) | AStmt::Let(_) => continue,
                AStmt::Const(stmt) => self.add_const(&stmt, loc),
                AStmt::Static(stmt) => self.add_static_var(&stmt, loc),
                AStmt::Fn(stmt) => self.add_fn(stmt, loc),
                AStmt::ExternFn(stmt) => self.add_extern_fn(stmt, loc),
                AStmt::Enum(stmt) => self.add_enum(stmt_id, stmt, PLoc::Stmt(stmt_id)),
                AStmt::Struct(stmt) => self.add_struct(stmt_id, stmt),
                AStmt::Use(stmt) => self.add_alias(stmt, loc),
            }
        }
    }
}

fn resolve_const_stmt(
    stmt: &AFieldLikeDecl,
    k_const: KConst,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let value_ty = ty_resolver.convert_ty_opt(stmt.ty_opt);
    k_const.of_mut(&mut mod_outline.consts).value_ty = value_ty;
}

fn resolve_static_stmt(
    stmt: &AFieldLikeDecl,
    static_var: KStaticVar,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let value_ty = ty_resolver.convert_ty_opt(stmt.ty_opt);
    static_var.of_mut(&mut mod_outline.static_vars).ty = value_ty;
}

fn new_field_loc(doc: Doc, parent: AVariantDeclKey, index: usize) -> Loc {
    Loc::new(doc, PLoc::FieldDecl(AFieldDeclKey::new(parent, index)))
}

fn resolve_record_variant_decl(
    decl: &ARecordVariantDecl,
    k_struct: KStruct,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let fields = k_struct.fields(&mod_outline.structs).to_owned();
    for (field_decl, field) in decl.fields.iter().zip(fields) {
        let ty = ty_resolver.convert_ty_opt(field_decl.ty_opt);
        field.of_mut(&mut mod_outline.fields).ty = ty;
    }
}

fn resolve_variant_decl(
    variant_decl: &AVariantDecl,
    k_struct: KStruct,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    match variant_decl {
        AVariantDecl::Const(_) => {}
        AVariantDecl::Record(decl) => {
            resolve_record_variant_decl(decl, k_struct, ty_resolver, mod_outline);
        }
    }
}

fn resolve_const_enum_stmt(
    stmt: &AEnumStmt,
    const_enum: KConstEnum,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = const_enum.variants(&mod_outline.const_enums).to_owned();
    for (stmt, k_const) in stmt.variants.iter().zip(variants.iter()) {
        let stmt = stmt.as_const().unwrap();
        let value_ty = ty_resolver.convert_ty_opt(stmt.ty_opt);
        k_const.of_mut(&mut mod_outline.consts).value_ty = value_ty;
    }
}

fn resolve_struct_enum_stmt(
    stmt: &AEnumStmt,
    struct_enum: KStructEnum,
    ty_resolver: &TyResolver,
    mod_outline: &mut KModOutline,
) {
    let variants = struct_enum.variants(&mod_outline.struct_enums).to_owned();
    for (variant_decl, k_struct) in stmt.variants.iter().zip(variants) {
        resolve_variant_decl(variant_decl, k_struct, ty_resolver, mod_outline);
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
    let ty_resolver = &TyResolver {
        ast,
        name_referents,
        name_symbols,
        logger,
    };

    for stmt in ast.stmts().iter() {
        let symbol = match stmt
            .name_opt()
            .and_then(|name| name_symbols.get(&name).cloned())
        {
            Some(it) => it,
            None => continue,
        };

        match stmt {
            AStmt::Attr | AStmt::Expr(_) | AStmt::Let(_) => continue,
            AStmt::Const(stmt) => {
                let k_const = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::Const(it)) => it,
                    _ => unreachable!(),
                };
                resolve_const_stmt(stmt, k_const, ty_resolver, mod_outline);
            }
            AStmt::Static(stmt) => {
                let static_var = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::StaticVar(it)) => it,
                    _ => unreachable!(),
                };
                resolve_static_stmt(stmt, static_var, ty_resolver, mod_outline);
            }
            AStmt::Fn(stmt) => {
                let k_fn = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::Fn(it)) => it,
                    _ => unreachable!(),
                };

                let param_tys = ty_resolver.convert_param_tys(&stmt.params);
                let result_ty = ty_resolver.convert_ty_or_unit(stmt.result_ty_opt);

                let fn_data = k_fn.of_mut(&mut mod_outline.fns);
                fn_data.param_tys = param_tys;
                fn_data.result_ty = result_ty;
            }
            AStmt::ExternFn(stmt) => {
                let extern_fn = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::ExternFn(it)) => it,
                    _ => unreachable!(),
                };

                let param_tys = ty_resolver.convert_param_tys(&stmt.params);
                let result_ty = ty_resolver.convert_ty_or_unit(stmt.result_ty_opt);

                let extern_fn_data = extern_fn.of_mut(&mut mod_outline.extern_fns);
                extern_fn_data.param_tys = param_tys;
                extern_fn_data.result_ty = result_ty;
            }
            AStmt::Enum(stmt) => match symbol {
                NameSymbol::ModSymbol(KModSymbol::ConstEnum(const_enum)) => {
                    resolve_const_enum_stmt(stmt, const_enum, ty_resolver, mod_outline)
                }
                NameSymbol::ModSymbol(KModSymbol::StructEnum(struct_enum)) => {
                    resolve_struct_enum_stmt(stmt, struct_enum, ty_resolver, mod_outline)
                }
                _ => unreachable!(),
            },
            AStmt::Struct(stmt) => {
                let variant_decl = match &stmt.variant_opt {
                    Some(it) => it,
                    None => continue,
                };
                let k_struct = match symbol {
                    NameSymbol::ModSymbol(KModSymbol::Struct(it)) => it,
                    _ => continue,
                };
                resolve_variant_decl(variant_decl, k_struct, ty_resolver, mod_outline);
            }
            AStmt::Use(_) => {}
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
