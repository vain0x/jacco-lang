//! 名前解決の処理

use super::*;
use crate::{logs::Logger, utils::IdProvider};
use std::{
    collections::HashMap,
    mem::{replace, take},
};

/// 関数の定義に関する情報
#[derive(Default)]
pub(crate) struct NFnData {
    pub(crate) fn_name_id_opt: Option<PNameId>,
}

/// 外部関数の定義に関する情報
#[derive(Default)]
pub(crate) struct NExternFnData {
    pub(crate) extern_fn_name_id_opt: Option<PNameId>,
}

pub(crate) struct NStructData {
    pub(crate) struct_name_id_opt: Option<PNameId>,
}

pub(crate) struct NFieldData {
    pub(crate) field_name_id_opt: Option<PNameId>,
}

/// 名前解決の結果。
pub(crate) struct NameResolution {
    pub(crate) fns: Vec<NFnData>,
    pub(crate) extern_fns: Vec<NExternFnData>,
    pub(crate) structs: Vec<NStructData>,
    pub(crate) fields: Vec<NFieldData>,
}

/// Naming context.
#[derive(Default)]
struct Nx {
    ids: IdProvider,
    env: HashMap<String, PNameInfo>,
    parent_loop: Option<usize>,
    parent_fn: Option<usize>,
    fns: Vec<NFnData>,
    extern_fns: Vec<NExternFnData>,
    structs: Vec<NStructData>,
    fields: Vec<NFieldData>,
    logger: Logger,
}

impl Nx {
    fn new(logger: Logger) -> Self {
        Self {
            logger,
            ..Self::default()
        }
    }

    fn fresh_id(&mut self) -> PNameId {
        self.ids.next()
    }

    fn alloc_fn(&mut self) -> usize {
        let fn_id = self.fns.len();
        self.fns.push(NFnData::default());
        fn_id
    }

    fn alloc_extern_fn(&mut self) -> PNameId {
        let extern_fn_id = self.extern_fns.len();
        self.extern_fns.push(NExternFnData::default());
        extern_fn_id
    }

    fn enter_scope(&mut self, do_resolve: impl FnOnce(&mut Nx)) {
        // FIXME: 効率化
        let outer_env = self.env.clone();

        do_resolve(self);

        self.env = outer_env;
    }

    fn enter_loop(&mut self, do_resolve: impl FnOnce(&mut Nx, usize)) {
        let loop_id = self.fresh_id();
        let parent_loop = replace(&mut self.parent_loop, Some(loop_id));

        do_resolve(self, loop_id);

        self.parent_loop = parent_loop;
    }

    fn enter_fn(&mut self, fn_id: usize, do_resolve: impl FnOnce(&mut Nx)) {
        let parent_loop = take(&mut self.parent_loop);
        let parent_fn = replace(&mut self.parent_fn, Some(fn_id));

        do_resolve(self);

        self.parent_loop = parent_loop;
        self.parent_fn = parent_fn;
    }
}

fn resolve_name_use(name: &mut PName, nx: &mut Nx) -> bool {
    match nx.env.get(name.text()) {
        Some(info) => {
            name.info_opt = Some(info.clone());
            true
        }
        None => false,
    }
}

fn resolve_name_def(name: &mut PName, kind: PNameKind, nx: &mut Nx) {
    let id = nx.fresh_id();
    let info = PNameInfo::new(id, kind);
    name.info_opt = Some(info.clone());
    nx.env.insert(name.text().to_string(), info);
}

fn resolve_ty_name(ty_name: &mut PNameTy, nx: &mut Nx) {
    let name = &mut ty_name.0;
    match name.text() {
        "i32" => {
            name.info_opt = Some(PNameInfo::I32);
        }
        "i64" => {
            name.info_opt = Some(PNameInfo::I64);
        }
        "usize" => {
            name.info_opt = Some(PNameInfo::USIZE);
        }
        "f64" => {
            name.info_opt = Some(PNameInfo::F64);
        }
        "c8" => {
            name.info_opt = Some(PNameInfo::C8);
        }
        "bool" => {
            name.info_opt = Some(PNameInfo::BOOL);
        }
        _ => {
            if !resolve_name_use(name, nx) {
                nx.logger.error(name, "undefined type");
                name.info_opt = Some(PNameInfo::UNRESOLVED);
            }
        }
    }

    assert!(name.info_opt.is_some());
}

fn resolve_ty(ty: &mut PTy, nx: &mut Nx) {
    match ty {
        PTy::Name(name) => resolve_ty_name(name, nx),
        PTy::Never(_) | PTy::Unit(_) => {}
        PTy::Ptr(PPtrTy { ty_opt, .. }) => {
            resolve_ty_opt(ty_opt.as_deref_mut(), nx);
        }
    }
}

fn resolve_ty_opt(ty_opt: Option<&mut PTy>, nx: &mut Nx) {
    if let Some(ty) = ty_opt {
        resolve_ty(ty, nx);
    }
}

fn resolve_pat(name: &mut PName, nx: &mut Nx) {
    resolve_name_def(name, PNameKind::Local, nx);
}

fn resolve_pat_opt(pat_opt: Option<&mut PName>, nx: &mut Nx) {
    if let Some(pat) = pat_opt {
        resolve_pat(pat, nx);
    }
}

fn resolve_expr(expr: &mut PExpr, nx: &mut Nx) {
    match expr {
        PExpr::Int(_)
        | PExpr::Float(_)
        | PExpr::Char(_)
        | PExpr::Str(_)
        | PExpr::True(_)
        | PExpr::False(_) => {}
        PExpr::Name(PNameExpr(name)) => {
            if !resolve_name_use(name, nx) {
                nx.logger.error(name, "undefined");
                name.info_opt = Some(PNameInfo::UNRESOLVED);
            }
        }
        PExpr::Struct(PStructExpr { name, fields, .. }) => {
            resolve_ty_name(name, nx);

            for field in fields {
                resolve_expr_opt(field.value_opt.as_mut(), nx);
            }
        }
        PExpr::Tuple(PTupleExpr { arg_list }) => {
            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::DotField(PDotFieldExpr { left, .. }) => {
            // NOTE: フィールド名は型検査が終わるまで解決できない。
            resolve_expr(left, nx);
        }
        PExpr::Call(PCallExpr { left, arg_list }) => {
            resolve_expr(left, nx);

            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::As(PAsExpr { left, ty_opt, .. }) => {
            resolve_expr(left, nx);
            resolve_ty_opt(ty_opt.as_mut(), nx);
        }
        PExpr::UnaryOp(PUnaryOpExpr { arg_opt, .. }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);
        }
        PExpr::BinaryOp(PBinaryOpExpr {
            left, right_opt, ..
        }) => {
            resolve_expr(left, nx);
            resolve_expr_opt(right_opt.as_deref_mut(), nx);
        }
        PExpr::Pipe(PPipeExpr {
            left, right_opt, ..
        }) => {
            resolve_expr(left, nx);
            resolve_expr_opt(right_opt.as_deref_mut(), nx);
        }
        PExpr::Block(PBlockExpr(block)) => {
            resolve_block(block, nx);
        }
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt,
            loop_id_opt,
        }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);

            *loop_id_opt = nx.parent_loop;
            if loop_id_opt.is_none() {
                nx.logger.error(keyword, "break out of loop");
            }
        }
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt,
        }) => {
            *loop_id_opt = nx.parent_loop;
            if loop_id_opt.is_none() {
                nx.logger.error(keyword, "continue out of loop");
            }
        }
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt,
            fn_id_opt,
        }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);

            *fn_id_opt = nx.parent_fn;
            if fn_id_opt.is_none() {
                nx.logger.error(keyword, "return out of function");
            }
        }
        PExpr::If(PIfExpr {
            cond_opt,
            body_opt,
            alt_opt,
            ..
        }) => {
            resolve_expr_opt(cond_opt.as_deref_mut(), nx);
            resolve_block_opt(body_opt.as_mut(), nx);
            resolve_expr_opt(alt_opt.as_deref_mut(), nx);
        }
        PExpr::While(PWhileExpr {
            cond_opt,
            body_opt,
            loop_id_opt,
            ..
        }) => {
            resolve_expr_opt(cond_opt.as_deref_mut(), nx);

            nx.enter_loop(|nx, loop_id| {
                *loop_id_opt = Some(loop_id);
                resolve_block_opt(body_opt.as_mut(), nx);
            });
        }
        PExpr::Loop(PLoopExpr {
            body_opt,
            loop_id_opt,
            ..
        }) => {
            nx.enter_loop(|nx, loop_id| {
                *loop_id_opt = Some(loop_id);
                resolve_block_opt(body_opt.as_mut(), nx);
            });
        }
    }
}

fn resolve_param_list_opt(param_list_opt: Option<&mut PParamList>, nx: &mut Nx) {
    let params = param_list_opt
        .into_iter()
        .flat_map(|param_list| param_list.params.iter_mut());
    for param in params {
        resolve_pat(&mut param.name, nx);
        resolve_ty_opt(param.ty_opt.as_mut(), nx);
    }
}

fn resolve_decls(decls: &mut [PDecl], nx: &mut Nx) {
    // 再帰的に定義される宣言を先に環境に加える。
    // FIXME: 同じスコープに同じ名前の再帰的な宣言があったらコンパイルエラー
    for decl in decls.iter_mut() {
        match decl {
            PDecl::Fn(PFnDecl {
                name_opt,
                fn_id_opt,
                ..
            }) => {
                let fn_id = nx.alloc_fn();
                *fn_id_opt = Some(fn_id);

                if let Some(name) = name_opt {
                    resolve_name_def(name, PNameKind::Fn, nx);

                    nx.fns[fn_id].fn_name_id_opt = name.info_opt.as_ref().map(|info| info.id());
                }
            }
            PDecl::ExternFn(PExternFnDecl {
                name_opt,
                extern_fn_id_opt,
                ..
            }) => {
                let extern_fn_id = nx.alloc_extern_fn();
                *extern_fn_id_opt = Some(extern_fn_id);

                if let Some(name) = name_opt.as_mut() {
                    resolve_name_def(name, PNameKind::ExternFn, nx);

                    nx.extern_fns[extern_fn_id].extern_fn_name_id_opt =
                        name.info_opt.as_ref().map(|info| info.id());
                }
            }
            PDecl::Struct(PStructDecl {
                name_opt,
                variant_opt,
                ..
            }) => {
                let mut struct_name_id_opt = None;

                if let Some(name) = name_opt.as_mut() {
                    resolve_name_def(name, PNameKind::Struct, nx);

                    struct_name_id_opt = name.info_opt.as_ref().map(|info| info.id());
                }

                nx.enter_scope(|nx| match variant_opt {
                    Some(PVariantDecl::Struct(PStructVariantDecl { fields, .. })) => {
                        for field in fields {
                            resolve_name_def(&mut field.name, PNameKind::Field, nx);
                            resolve_ty_opt(field.ty_opt.as_mut(), nx);

                            let field_id = nx.fields.len();
                            nx.fields.push(NFieldData {
                                field_name_id_opt: field
                                    .name
                                    .info_opt
                                    .as_ref()
                                    .map(|info| info.id()),
                            });
                            field.field_id_opt = Some(field_id);
                        }
                    }
                    None => {}
                });

                nx.structs.push(NStructData { struct_name_id_opt });
            }
            PDecl::Expr(_) | PDecl::Let(_) | PDecl::Const(_) | PDecl::Static(_) => {}
        }
    }

    for decl in decls.iter_mut() {
        resolve_decl(decl, nx);
    }
}

fn resolve_decl(decl: &mut PDecl, nx: &mut Nx) {
    match decl {
        PDecl::Expr(PExprDecl { expr, .. }) => {
            resolve_expr(expr, nx);
        }
        PDecl::Let(PLetDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);
            resolve_pat_opt(name_opt.as_mut(), nx)
        }
        PDecl::Const(PConstDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt {
                resolve_name_def(name, PNameKind::Const, nx);
            }
        }
        PDecl::Static(PStaticDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_expr_opt(init_opt.as_mut(), nx);

            if let Some(name) = name_opt {
                resolve_name_def(name, PNameKind::StaticVar, nx);
            }
        }
        PDecl::Fn(PFnDecl {
            param_list_opt,
            result_ty_opt,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            let fn_id = fn_id_opt.unwrap();

            nx.enter_fn(fn_id, |nx| {
                nx.enter_scope(|nx| {
                    resolve_param_list_opt(param_list_opt.as_mut(), nx);
                    resolve_ty_opt(result_ty_opt.as_mut(), nx);

                    resolve_block_opt(block_opt.as_mut(), nx);
                });
            });
        }
        PDecl::ExternFn(PExternFnDecl {
            param_list_opt,
            result_ty_opt,
            ..
        }) => {
            nx.enter_scope(|nx| {
                resolve_param_list_opt(param_list_opt.as_mut(), nx);
                resolve_ty_opt(result_ty_opt.as_mut(), nx);
            });
        }
        PDecl::Struct(_) => {}
    }
}

fn resolve_expr_opt(expr_opt: Option<&mut PExpr>, nx: &mut Nx) {
    if let Some(expr) = expr_opt {
        resolve_expr(expr, nx);
    }
}

fn resolve_block(block: &mut PBlock, nx: &mut Nx) {
    nx.enter_scope(|nx| {
        resolve_decls(&mut block.decls, nx);

        if let Some(last) = &mut block.last_opt {
            resolve_expr(last, nx);
        }
    });
}

fn resolve_block_opt(block_opt: Option<&mut PBlock>, nx: &mut Nx) {
    if let Some(block) = block_opt {
        resolve_block(block, nx);
    }
}

pub(crate) fn resolve_name(p_root: &mut PRoot, logger: Logger) -> NameResolution {
    let mut nx = Nx::new(logger);

    resolve_decls(&mut p_root.decls, &mut nx);

    NameResolution {
        fns: nx.fns,
        extern_fns: nx.extern_fns,
        structs: nx.structs,
        fields: nx.fields,
    }
}
