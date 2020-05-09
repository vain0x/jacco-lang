//! 名前解決の処理

use super::*;
use std::collections::HashMap;

/// Naming context.
#[derive(Default)]
struct Nx {
    last_id: PNameId,
    env: HashMap<String, PNameId>,
}

impl Nx {
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

fn resolve_expr(expr: &mut PExpr, nx: &mut Nx) {
    match expr {
        PExpr::Int(_) | PExpr::Str(_) => {}
        PExpr::Name(name) => match nx.env.get(&name.text) {
            Some(name_id) => {
                name.name_id = *name_id;
            }
            None => {
                eprintln!("undefined {:?}", name);
            }
        },
        PExpr::Call { callee, arg_list } => {
            resolve_expr(callee, nx);
            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.expr, nx);
            }
        }
        PExpr::UnaryOp { arg_opt, .. } => {
            if let Some(arg) = arg_opt {
                resolve_expr(arg, nx);
            }
        }
        PExpr::BinaryOp {
            left, right_opt, ..
        } => {
            resolve_expr(left, nx);

            if let Some(right) = right_opt {
                resolve_expr(right, nx);
            }
        }
        PExpr::Block(block) => {
            resolve_block(block, nx);
        }
        PExpr::Break { arg_opt, .. } | PExpr::Return { arg_opt, .. } => {
            if let Some(arg) = arg_opt {
                resolve_expr(arg, nx);
            }
        }
        PExpr::If {
            cond_opt,
            body_opt,
            alt_opt,
            ..
        } => {
            if let Some(cond) = cond_opt {
                resolve_expr(cond, nx);
            }

            if let Some(block) = body_opt {
                nx.enter_scope(|nx| {
                    resolve_block(block, nx);
                });
            }

            if let Some(alt) = alt_opt {
                nx.enter_scope(|nx| {
                    resolve_expr(alt, nx);
                });
            }
        }
        PExpr::While {
            cond_opt, body_opt, ..
        } => {
            if let Some(cond) = cond_opt {
                resolve_expr(cond, nx);
            }

            if let Some(block) = body_opt {
                nx.enter_scope(|nx| {
                    resolve_block(block, nx);
                });
            }
        }
        PExpr::Loop { body_opt, .. } => {
            if let Some(block) = body_opt {
                nx.enter_scope(|nx| {
                    resolve_block(block, nx);
                });
            }
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
            if let Some(init) = init_opt {
                resolve_expr(init, nx);
            }

            if let Some(name) = name_opt {
                resolve_pat(name, nx);
            }
        }
        PDecl::Fn { block_opt, .. } => {
            if let Some(block) = block_opt {
                nx.enter_scope(|nx| {
                    for decl in &mut block.decls {
                        resolve_decl(decl, nx);
                    }

                    if let Some(last) = &mut block.last_opt {
                        resolve_expr(last, nx);
                    }
                });
            }
        }
        PDecl::ExternFn { param_list_opt, .. } => {
            if let Some(params) = param_list_opt
                .as_mut()
                .map(|param_list| &mut param_list.params)
            {
                for param in params {
                    resolve_pat(&mut param.name, nx);
                }
            }
        }
    }
}

fn resolve_block(block: &mut PBlock, nx: &mut Nx) {
    for decl in &mut block.decls {
        resolve_decl(decl, nx);
    }

    if let Some(last) = &mut block.last_opt {
        resolve_expr(last, nx);
    }
}

pub(crate) fn resolve_name(p_root: &mut PRoot) {
    let mut nx = Nx::default();

    for decl in &mut p_root.decls {
        resolve_decl(decl, &mut nx);
    }
}
