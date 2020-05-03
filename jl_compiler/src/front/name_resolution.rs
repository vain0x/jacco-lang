use super::*;
use std::collections::HashMap;

/// Naming context.
#[derive(Default)]
struct Nx {
    env: HashMap<String, PNameId>,
}

impl Nx {
    fn enter_scope(&mut self, mut do_resolve: impl FnMut(&mut Nx)) {
        let outer_env = self.env.clone();

        do_resolve(self);

        self.env = outer_env;
    }
}

fn resolve_expr(expr: &mut PTerm, nx: &mut Nx) {
    match expr {
        PTerm::Int(_) | PTerm::Str(_) => {}
        PTerm::Name(name) => match nx.env.get(&name.text) {
            Some(name_id) => {
                name.name_id = *name_id;
            }
            None => {
                eprintln!("undefined {:?}", name);
            }
        },
        PTerm::Call { callee, arg_list } => {
            resolve_expr(callee, nx);
            for arg in &mut arg_list.args {
                resolve_expr(&mut arg.term, nx);
            }
        }
        PTerm::BinaryOp { left, right, .. } => {
            resolve_expr(left, nx);
            resolve_expr(right, nx);
        }
    }
}

fn resolve_block(block: &mut PBlock, nx: &mut Nx) {
    for stmt in &mut block.body {
        resolve_stmt(stmt, nx);
    }

    if let Some(last) = &mut block.last_opt {
        resolve_expr(last, nx);
    }
}

fn resolve_stmt(stmt: &mut PStmt, nx: &mut Nx) {
    match stmt {
        PStmt::Expr { term, .. } => {
            resolve_expr(term, nx);
        }
        PStmt::Block(block) => {
            resolve_block(block, nx);
        }
        PStmt::If {
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
                    resolve_stmt(alt, nx);
                });
            }
        }
        PStmt::While {
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
        PStmt::Loop { body_opt, .. } => {
            if let Some(block) = body_opt {
                nx.enter_scope(|nx| {
                    resolve_block(block, nx);
                });
            }
        }
        PStmt::Let {
            name_opt, init_opt, ..
        } => {
            if let Some(init) = init_opt {
                resolve_expr(init, nx);
            }

            if let Some(name) = name_opt {
                nx.env.insert(name.text.clone(), name.name_id);
            }
        }
        PStmt::Fn { block_opt, .. } => {
            if let Some(block) = block_opt {
                nx.enter_scope(|nx| {
                    for stmt in &mut block.body {
                        resolve_stmt(stmt, nx);
                    }

                    if let Some(last) = &mut block.last_opt {
                        resolve_expr(last, nx);
                    }
                });
            }
        }
        PStmt::Break { .. } | PStmt::Continue { .. } | PStmt::ExternFn { .. } => {}
    }
}

pub(crate) fn resolve_name(p_root: &mut PRoot) {
    let mut nx = Nx::default();

    for stmt in &mut p_root.body {
        resolve_stmt(stmt, &mut nx);
    }
}
