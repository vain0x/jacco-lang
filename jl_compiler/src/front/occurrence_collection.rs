//! シンボルの出現箇所を収集する。

use super::*;
use std::{collections::HashMap, mem::replace, rc::Rc};

#[derive(Default)]
pub(crate) struct Occurrences {
    pub(crate) def_sites: HashMap<(NName, NName), Vec<Location>>,
    pub(crate) use_sites: HashMap<(NName, NName), Vec<Location>>,
    pub(crate) field_uses: Vec<(String, Location)>,
}

/// Collecting context.
struct Cx {
    #[allow(unused)]
    res: Rc<NameResolution>,
    parent_fn: NName,
    occurrences: Occurrences,
}

impl Cx {
    fn new(res: Rc<NameResolution>) -> Self {
        Self {
            res,
            parent_fn: NName::Unresolved,
            occurrences: Occurrences::default(),
        }
    }
}

fn full_name(n_name: NName, cx: &Cx) -> (NName, NName) {
    let parent = match n_name {
        NName::LocalVar(_) => cx.parent_fn,
        _ => NName::Unresolved,
    };
    (parent, n_name)
}

fn resolve_name_def(p_name: &PName, cx: &mut Cx) {
    if let Some(n_name) = p_name.info_opt.clone() {
        cx.occurrences
            .def_sites
            .entry(full_name(n_name, cx))
            .or_insert(vec![])
            .push(p_name.location());
    }
}

fn resolve_name_use(p_name: &PName, cx: &mut Cx) {
    if let Some(n_name) = p_name.info_opt.clone() {
        cx.occurrences
            .use_sites
            .entry(full_name(n_name, cx))
            .or_insert(vec![])
            .push(p_name.location());
    }
}

fn resolve_ty_name(ty_name: &PName, cx: &mut Cx) {
    resolve_name_use(&ty_name, cx);
}

fn resolve_ty(ty: &PTy, cx: &mut Cx) {
    match ty {
        PTy::Name(name) => resolve_ty_name(name, cx),
        PTy::Never(_) | PTy::Unit(_) => {}
        PTy::Ptr(PPtrTy { ty_opt, .. }) => {
            resolve_ty_opt(ty_opt.as_deref(), cx);
        }
    }
}

fn resolve_ty_opt(ty_opt: Option<&PTy>, cx: &mut Cx) {
    if let Some(ty) = ty_opt {
        resolve_ty(ty, cx);
    }
}

fn resolve_pat(pat: &PPat, cx: &mut Cx) {
    match pat {
        PPat::Name(name) => resolve_name_def(name, cx),
        PPat::Record(PRecordPat { name, .. }) => resolve_name_def(name, cx),
    }
}

#[allow(unused)]
fn resolve_pat_opt(pat_opt: Option<&PPat>, cx: &mut Cx) {
    if let Some(pat) = pat_opt {
        resolve_pat(pat, cx);
    }
}

fn resolve_expr(expr: &PExpr, cx: &mut Cx) {
    match expr {
        PExpr::Int(_)
        | PExpr::Float(_)
        | PExpr::Char(_)
        | PExpr::Str(_)
        | PExpr::True(_)
        | PExpr::False(_) => {}
        PExpr::Name(name) => {
            resolve_name_use(name, cx);
        }
        PExpr::Record(PRecordExpr { name, fields, .. }) => {
            resolve_ty_name(name, cx);

            for field in fields {
                cx.occurrences
                    .field_uses
                    .push((field.name.text().to_string(), field.location()));

                resolve_expr_opt(field.value_opt.as_ref(), cx);
            }
        }
        PExpr::Tuple(PTupleExpr { arg_list }) => {
            for arg in &arg_list.args {
                resolve_expr(&arg.expr, cx);
            }
        }
        PExpr::DotField(PDotFieldExpr { left, name_opt, .. }) => {
            resolve_expr(left, cx);

            if let Some(field) = name_opt {
                cx.occurrences
                    .field_uses
                    .push((field.text().to_string(), field.location()));
            }
        }
        PExpr::Call(PCallExpr { left, arg_list }) | PExpr::Index(PIndexExpr { left, arg_list }) => {
            resolve_expr(left, cx);

            for arg in &arg_list.args {
                resolve_expr(&arg.expr, cx);
            }
        }
        PExpr::As(PAsExpr { left, ty_opt, .. }) => {
            resolve_expr(left, cx);
            resolve_ty_opt(ty_opt.as_ref(), cx);
        }
        PExpr::UnaryOp(PUnaryOpExpr { arg_opt, .. }) => {
            resolve_expr_opt(arg_opt.as_deref(), cx);
        }
        PExpr::BinaryOp(PBinaryOpExpr {
            left, right_opt, ..
        }) => {
            resolve_expr(left, cx);
            resolve_expr_opt(right_opt.as_deref(), cx);
        }
        PExpr::Pipe(PPipeExpr {
            left, right_opt, ..
        }) => {
            resolve_expr(left, cx);
            resolve_expr_opt(right_opt.as_deref(), cx);
        }
        PExpr::Block(PBlockExpr(block)) => {
            resolve_block(block, cx);
        }
        PExpr::Break(PBreakExpr { arg_opt, .. }) => {
            resolve_expr_opt(arg_opt.as_deref(), cx);
        }
        PExpr::Continue(PContinueExpr { .. }) => {}
        PExpr::Return(PReturnExpr { arg_opt, .. }) => {
            resolve_expr_opt(arg_opt.as_deref(), cx);
        }
        PExpr::If(PIfExpr {
            cond_opt,
            body_opt,
            alt_opt,
            ..
        }) => {
            resolve_expr_opt(cond_opt.as_deref(), cx);
            resolve_block_opt(body_opt.as_ref(), cx);
            resolve_expr_opt(alt_opt.as_deref(), cx);
        }
        PExpr::Match(PMatchExpr { cond_opt, arms, .. }) => {
            resolve_expr_opt(cond_opt.as_deref(), cx);

            for arm in arms {
                resolve_pat(&arm.pat, cx);
                resolve_expr_opt(arm.body_opt.as_deref(), cx);
            }
        }
        PExpr::While(PWhileExpr {
            cond_opt, body_opt, ..
        }) => {
            resolve_expr_opt(cond_opt.as_deref(), cx);
            resolve_block_opt(body_opt.as_ref(), cx);
        }
        PExpr::Loop(PLoopExpr { body_opt, .. }) => {
            resolve_block_opt(body_opt.as_ref(), cx);
        }
    }
}

fn resolve_param_list_opt(param_list_opt: Option<&PParamList>, cx: &mut Cx) {
    let params = param_list_opt
        .into_iter()
        .flat_map(|param_list| param_list.params.iter());
    for param in params {
        resolve_name_def(&param.name, cx);
        resolve_ty_opt(param.ty_opt.as_ref(), cx);
    }
}

fn resolve_variant(variant: &PVariantDecl, cx: &mut Cx) {
    match variant {
        PVariantDecl::Const(PConstVariantDecl {
            name, value_opt, ..
        }) => {
            resolve_name_def(name, cx);
            resolve_expr_opt(value_opt.as_deref(), cx);
        }
        PVariantDecl::Record(PRecordVariantDecl { name, fields, .. }) => {
            resolve_name_def(name, cx);

            for field in fields {
                resolve_name_def(&field.name, cx);
                resolve_ty_opt(field.ty_opt.as_ref(), cx);
            }
        }
    }
}

fn resolve_decls(decls: &[PDecl], cx: &mut Cx) {
    for decl in decls {
        resolve_decl(decl, cx);
    }
}

fn resolve_decl(decl: &PDecl, cx: &mut Cx) {
    match decl {
        PDecl::Expr(PExprDecl { expr, .. }) => {
            resolve_expr(expr, cx);
        }
        PDecl::Let(PLetDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_ref(), cx);
            resolve_expr_opt(init_opt.as_ref(), cx);

            if let Some(name) = name_opt {
                resolve_name_def(name, cx);
            }
        }
        PDecl::Const(PConstDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_ref(), cx);
            resolve_expr_opt(init_opt.as_ref(), cx);

            if let Some(name) = name_opt {
                resolve_name_def(name, cx);
            }
        }
        PDecl::Static(PStaticDecl {
            name_opt,
            ty_opt,
            init_opt,
            ..
        }) => {
            resolve_ty_opt(ty_opt.as_ref(), cx);
            resolve_expr_opt(init_opt.as_ref(), cx);

            if let Some(name) = name_opt {
                resolve_name_def(name, cx);
            }
        }
        PDecl::Fn(PFnDecl {
            name_opt,
            param_list_opt,
            result_ty_opt,
            block_opt,
            ..
        }) => {
            let parent_fn = {
                let n_fn = name_opt
                    .as_ref()
                    .and_then(|name| name.info_opt)
                    .unwrap_or(NName::Unresolved);
                replace(&mut cx.parent_fn, n_fn)
            };

            if let Some(name) = name_opt {
                resolve_name_def(name, cx);
            }

            resolve_param_list_opt(param_list_opt.as_ref(), cx);
            resolve_ty_opt(result_ty_opt.as_ref(), cx);
            resolve_block_opt(block_opt.as_ref(), cx);

            cx.parent_fn = parent_fn;
        }
        PDecl::ExternFn(PExternFnDecl {
            name_opt,
            param_list_opt,
            result_ty_opt,
            ..
        }) => {
            let parent_fn = {
                let extern_fn = name_opt
                    .as_ref()
                    .and_then(|name| name.info_opt)
                    .unwrap_or(NName::Unresolved);
                replace(&mut cx.parent_fn, extern_fn)
            };

            if let Some(name) = name_opt {
                resolve_name_def(name, cx);
            }

            resolve_param_list_opt(param_list_opt.as_ref(), cx);
            resolve_ty_opt(result_ty_opt.as_ref(), cx);

            cx.parent_fn = parent_fn;
        }
        PDecl::Enum(PEnumDecl {
            name_opt, variants, ..
        }) => {
            if let Some(name) = name_opt {
                resolve_name_def(name, cx);
            }

            for variant in variants {
                resolve_variant(variant, cx);
            }
        }
        PDecl::Struct(PStructDecl { variant_opt, .. }) => {
            if let Some(variant) = variant_opt {
                resolve_variant(variant, cx);
            }
        }
    }
}

fn resolve_expr_opt(expr_opt: Option<&PExpr>, cx: &mut Cx) {
    if let Some(expr) = expr_opt {
        resolve_expr(expr, cx);
    }
}

fn resolve_block(block: &PBlock, cx: &mut Cx) {
    resolve_decls(&block.decls, cx);

    if let Some(last) = &block.last_opt {
        resolve_expr(last, cx);
    }
}

fn resolve_block_opt(block_opt: Option<&PBlock>, cx: &mut Cx) {
    if let Some(block) = block_opt {
        resolve_block(block, cx);
    }
}

pub(crate) fn collect_occurrences(p_root: &PRoot, res: Rc<NameResolution>) -> Occurrences {
    let mut cx = Cx::new(res);

    resolve_decls(&p_root.decls, &mut cx);

    cx.occurrences
}
