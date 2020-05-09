//! 名前解決の処理

use super::*;
use crate::logs::Logger;
use std::collections::HashMap;

/// Naming context.
#[derive(Default)]
struct Nx {
    last_id: PNameId,
    env: HashMap<String, PNameId>,
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
        self.last_id += 1;
        self.last_id
    }

    fn enter_scope(&mut self, mut do_resolve: impl FnMut(&mut Nx)) {
        let outer_env = self.env.clone();

        do_resolve(self);

        self.env = outer_env;
    }
}

fn resolve_pat(name: &mut PName, nx: &mut Nx) {
    name.name_id = nx.fresh_id();
    nx.env.insert(name.text.clone(), name.name_id);
}

fn resolve_pat_opt(pat_opt: Option<&mut PName>, nx: &mut Nx) {
    if let Some(pat) = pat_opt {
        resolve_pat(pat, nx);
    }
}

fn resolve_expr(expr: &mut PExpr, nx: &mut Nx) {
    match expr {
        PExpr::Int(_) | PExpr::Str(_) => {}
        PExpr::Name(name) => match nx.env.get(&name.text) {
            Some(name_id) => {
                name.name_id = *name_id;
            }
            None => {
                nx.logger.error(name.location.clone(), "undefined");
            }
        },
        PExpr::Tuple(arg_list) => {
            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::Call { callee, arg_list } => {
            resolve_expr(callee, nx);

            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::UnaryOp { arg_opt, .. } => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);
        }
        PExpr::BinaryOp {
            left, right_opt, ..
        } => {
            resolve_expr(left, nx);
            resolve_expr_opt(right_opt.as_deref_mut(), nx);
        }
        PExpr::Block(block) => {
            resolve_block(block, nx);
        }
        PExpr::Break { arg_opt, .. } | PExpr::Return { arg_opt, .. } => {
            resolve_expr_opt(arg_opt.as_deref_mut(), nx);
        }
        PExpr::If {
            cond_opt,
            body_opt,
            alt_opt,
            ..
        } => {
            resolve_expr_opt(cond_opt.as_deref_mut(), nx);
            resolve_block_opt(body_opt.as_mut(), nx);
            resolve_expr_opt(alt_opt.as_deref_mut(), nx);
        }
        PExpr::While {
            cond_opt, body_opt, ..
        } => {
            resolve_expr_opt(cond_opt.as_deref_mut(), nx);
            resolve_block_opt(body_opt.as_mut(), nx);
        }
        PExpr::Loop { body_opt, .. } => {
            resolve_block_opt(body_opt.as_mut(), nx);
        }
        PExpr::Continue { .. } => {}
    }
}

fn resolve_decl(decl: &mut PDecl, nx: &mut Nx) {
    match decl {
        PDecl::Expr { expr, .. } => {
            resolve_expr(expr, nx);
        }
        PDecl::Let {
            name_opt, init_opt, ..
        } => {
            resolve_expr_opt(init_opt.as_mut(), nx);
            resolve_pat_opt(name_opt.as_mut(), nx)
        }
        PDecl::Fn { block_opt, .. } => {
            resolve_block_opt(block_opt.as_mut(), nx);
        }
        PDecl::ExternFn { param_list_opt, .. } => {
            let params = param_list_opt
                .into_iter()
                .flat_map(|param_list| param_list.params.iter_mut());
            for param in params {
                resolve_pat(&mut param.name, nx);
            }
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
    })
}

fn resolve_block_opt(block_opt: Option<&mut PBlock>, nx: &mut Nx) {
    if let Some(block) = block_opt {
        resolve_block(block, nx);
    }
}

pub(crate) fn resolve_name(p_root: &mut PRoot, logger: Logger) {
    let mut nx = Nx::new(logger);

    for decl in &mut p_root.decls {
        resolve_decl(decl, &mut nx);
    }
}
