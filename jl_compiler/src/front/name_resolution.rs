//! 名前解決の処理

use super::*;
use crate::{logs::Logger, utils::IdProvider};
use std::collections::HashMap;
use std::mem::{replace, take};

/// 関数定義に関する情報
#[derive(Default)]
pub(crate) struct NFn {
    fn_name_id_opt: Option<PNameId>,
}

impl NFn {
    pub(crate) fn fn_name_id_opt(&self) -> Option<PNameId> {
        self.fn_name_id_opt
    }
}

/// 名前解決の結果。
pub(crate) struct NameResolution {
    fns: Vec<NFn>,
}

impl NameResolution {
    pub(crate) fn fns(&self) -> &[NFn] {
        &self.fns
    }
}

/// Naming context.
#[derive(Default)]
struct Nx {
    ids: IdProvider,
    env: HashMap<String, PNameInfo>,
    parent_loop: Option<usize>,
    parent_fn: Option<usize>,
    fns: Vec<NFn>,
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

    fn enter_scope(&mut self, do_resolve: impl FnOnce(&mut Nx)) {
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

    fn enter_fn(&mut self, do_resolve: impl FnOnce(&mut Nx, usize)) {
        let fn_id = self.fns.len();
        self.fns.push(NFn::default());

        let parent_loop = take(&mut self.parent_loop);
        let parent_fn = replace(&mut self.parent_fn, Some(fn_id));

        do_resolve(self, fn_id);

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
        PExpr::Int(_) | PExpr::Str(_) => {}
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
        PExpr::UnaryOp(PUnaryOpExpr { arg_opt, .. }) => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);
        }
        PExpr::BinaryOp(PBinaryOpExpr {
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
            resolve_expr_opt(init_opt.as_mut(), nx);
            resolve_ty_opt(ty_opt.as_mut(), nx);
            resolve_pat_opt(name_opt.as_mut(), nx)
        }
        PDecl::Fn(PFnDecl {
            name_opt,
            param_list_opt,
            result_ty_opt,
            block_opt,
            fn_id_opt,
            ..
        }) => {
            nx.enter_fn(|nx, fn_id| {
                *fn_id_opt = Some(fn_id);

                if let Some(name) = name_opt.as_mut() {
                    resolve_name_def(name, PNameKind::Fn, nx);
                    nx.fns[fn_id].fn_name_id_opt = name.info_opt.as_ref().map(|info| info.id());
                }

                nx.enter_scope(|nx| {
                    resolve_param_list_opt(param_list_opt.as_mut(), nx);
                    resolve_ty_opt(result_ty_opt.as_mut(), nx);

                    resolve_block_opt(block_opt.as_mut(), nx);
                });
            });
        }
        PDecl::ExternFn(PExternFnDecl {
            name_opt,
            param_list_opt,
            result_ty_opt,
            ..
        }) => {
            if let Some(name) = name_opt.as_mut() {
                resolve_name_def(name, PNameKind::ExternFn, nx);
            }

            nx.enter_scope(|nx| {
                resolve_param_list_opt(param_list_opt.as_mut(), nx);
                resolve_ty_opt(result_ty_opt.as_mut(), nx);
            });
        }
        PDecl::Struct(PStructDecl {
            name_opt,
            variant_opt,
            ..
        }) => {
            if let Some(name) = name_opt.as_mut() {
                resolve_name_def(name, PNameKind::Struct, nx);
            }

            nx.enter_scope(|nx| match variant_opt {
                Some(PVariantDecl::Struct(PStructVariantDecl { fields, .. })) => {
                    for field in fields {
                        resolve_name_def(&mut field.name, PNameKind::Field, nx);
                        resolve_ty_opt(field.ty_opt.as_mut(), nx);
                    }
                }
                None => {}
            });
        }
    }
}

fn resolve_expr_opt(expr_opt: Option<&mut PExpr>, nx: &mut Nx) {
    if let Some(expr) = expr_opt {
        resolve_expr(expr, nx);
    }
}

fn resolve_block(block: &mut PBlock, nx: &mut Nx) {
    nx.enter_scope(|nx| {
        for decl in &mut block.decls {
            resolve_decl(decl, nx);
        }

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

    for decl in &mut p_root.decls {
        resolve_decl(decl, &mut nx);
    }

    NameResolution { fns: nx.fns }
}
