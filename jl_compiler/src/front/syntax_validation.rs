use super::*;

struct Vx {
    logger: Logger,
}

impl Vx {
    fn new(logger: Logger) -> Self {
        Vx { logger }
    }
}

fn validate_param(param: &PParam, vx: &Vx) {
    match (&param.colon_opt, &param.ty_opt) {
        (Some(_), Some(ty)) => validate_ty(&ty, vx),
        _ => vx
            .logger
            .error(param.location().clone(), "maybe missed type ascription?"),
    }
}

fn validate_param_list(param_list: &PParamList, vx: &Vx) {
    for (i, param) in param_list.params.iter().enumerate() {
        validate_param(param, vx);

        let is_last = i + 1 == param_list.params.len();
        if !is_last && param.comma_opt.is_none() {
            vx.logger
                .error(param.location().clone(), "maybe missing following comma?");
        }
    }

    if param_list.right_opt.is_none() {
        vx.logger.error(
            param_list.location().clone(),
            "maybe missing closing paren?",
        );
    }
}

fn validate_result(arrow_opt: Option<&TokenData>, ty_opt: Option<&PTy>, vx: &Vx) {
    match (arrow_opt, ty_opt) {
        (Some(_), Some(ty)) => validate_ty(ty, vx),
        (Some(arrow), None) => vx
            .logger
            .error(arrow.location().clone(), "missed the type of result?"),
        (None, Some(_)) | (None, None) => {}
    }
}

fn validate_arg(arg: &PArg, vx: &Vx) {
    validate_expr(&arg.expr, vx);
}

fn validate_arg_list(arg_list: &PArgList, vx: &Vx) {
    for (i, arg) in arg_list.args.iter().enumerate() {
        validate_arg(arg, vx);

        let is_last = i + 1 == arg_list.args.len();
        if !is_last && arg.comma_opt.is_none() {
            vx.logger
                .error(arg.location().clone(), "maybe missed following comma?");
        }
    }

    if arg_list.right_opt.is_none() {
        vx.logger
            .error(arg_list.location().clone(), "maybe missed closing paren?");
    }
}

fn validate_ty(ty: &PTy, vx: &Vx) {
    match ty {
        PTy::Name(_) | PTy::Never { .. } => {}
        PTy::Unit(PUnitTy { right_opt, .. }) => {
            if right_opt.is_none() {
                vx.logger
                    .error(ty.location().clone(), "maybe missed closing paren?");
            }
        }
        PTy::Ptr(PPtrTy { ty_opt, .. }) => {
            if ty_opt.is_none() {
                vx.logger
                    .error(ty.location().clone(), "maybe missed following type?");
            }
        }
    }
}

fn validate_ty_opt(ty_opt: Option<&PTy>, vx: &Vx) {
    if let Some(ty) = ty_opt {
        validate_ty(ty, vx)
    }
}

fn validate_block(block: &PBlock, vx: &Vx) {
    for (i, decl) in block.decls.iter().enumerate() {
        let semi_required = {
            let is_last = i + 1 == block.decls.len();
            !is_last
        };

        validate_decl(decl, vx, Placement::Local, semi_required);
    }
}

fn validate_expr(expr: &PExpr, vx: &Vx) {
    match expr {
        PExpr::Int(_) => {
            // FIXME: should verify
        }
        PExpr::Str(_) => {
            // FIXME: should verify
        }
        PExpr::Name(_) => {}
        PExpr::Tuple(PTupleExpr { arg_list }) => validate_arg_list(arg_list, vx),
        PExpr::Call(PCallExpr { callee, arg_list }) => {
            validate_expr(&callee, vx);
            validate_arg_list(arg_list, vx);
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            arg_opt, location, ..
        }) => match arg_opt.as_deref() {
            Some(arg) => validate_expr(arg, vx),
            None => vx.logger.error(
                location.clone(),
                "maybe missed the argument of the unary operator?",
            ),
        },
        PExpr::BinaryOp(PBinaryOpExpr {
            left,
            right_opt,
            location,
            ..
        }) => {
            validate_expr(left, vx);

            match right_opt {
                Some(right) => validate_expr(right, vx),
                None => vx
                    .logger
                    .error(location.clone(), "maybe missed the right-hand side?"),
            }
        }
        PExpr::Block(PBlockExpr(block)) => {
            validate_block(block, vx);
        }
        PExpr::Break(PBreakExpr { arg_opt, .. }) => {
            validate_expr_opt(arg_opt.as_deref(), vx);
        }
        PExpr::Continue { .. } => {}
        PExpr::Return(PReturnExpr { arg_opt, .. }) => {
            validate_expr_opt(arg_opt.as_deref(), vx);
        }
        PExpr::If(PIfExpr {
            keyword,
            cond_opt,
            body_opt,
            else_opt,
            alt_opt,
        }) => {
            match cond_opt {
                Some(cond) => validate_expr(cond, vx),
                None => vx.logger.error(
                    keyword.location().clone(),
                    "maybe missed condition of the if expression?",
                ),
            }

            match body_opt {
                Some(body) => validate_block(body, vx),
                None => vx.logger.error(
                    keyword.location().clone(),
                    "maybe missed body of the if expression?",
                ),
            }

            match (else_opt, alt_opt) {
                (Some(_), Some(alt)) => validate_expr(alt, vx),
                (Some(else_keyword), None) => vx.logger.error(
                    else_keyword.location().clone(),
                    "maybe missed the body of the else clause?",
                ),
                (None, Some(_)) => {
                    // unreachable
                }
                (None, None) => {}
            }
        }
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt,
            body_opt,
        }) => {
            match cond_opt {
                Some(cond) => validate_expr(cond, vx),
                None => vx.logger.error(
                    keyword.location().clone(),
                    "maybe missed the condition of the while expression?",
                ),
            }

            match body_opt {
                Some(body) => validate_block(body, vx),
                None => vx.logger.error(
                    keyword.location().clone(),
                    "maybe missed the body of the while expression?",
                ),
            }
        }
        PExpr::Loop(PLoopExpr { keyword, body_opt }) => match body_opt {
            Some(body) => validate_block(body, vx),
            None => vx.logger.error(
                keyword.location().clone(),
                "maybe missed body of the if expression?",
            ),
        },
    }
}

fn validate_expr_opt(expr_opt: Option<&PExpr>, vx: &Vx) {
    if let Some(expr) = expr_opt {
        validate_expr(expr, vx);
    }
}

fn validate_decl(decl: &PDecl, vx: &Vx, placement: Placement, semi_required: bool) {
    match (decl, placement) {
        (PDecl::Expr { .. }, Placement::Global) | (PDecl::Let { .. }, Placement::Global) => {
            vx.logger.error(decl.location().clone(), "not allowed");
        }
        _ => {}
    }

    match decl {
        PDecl::Expr(PExprDecl { expr, semi_opt }) => {
            validate_expr(expr, vx);

            if semi_required && semi_opt.is_none() && !expr.ends_with_block() {
                vx.logger
                    .error(decl.location().behind(), "missed a semicolon?");
            }
        }
        PDecl::Let(PLetDecl {
            keyword,
            name_opt,
            ty_opt,
            equal_opt,
            init_opt,
            semi_opt,
            ..
        }) => {
            if name_opt.is_none() {
                vx.logger
                    .error(keyword.location().clone(), "missed variable name?");
            }

            validate_ty_opt(ty_opt.as_ref(), vx);

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                (Some(equal), None) => vx
                    .logger
                    .error(equal.location().clone(), "missed an expression?"),
                (None, Some(_)) | (None, None) => {}
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                vx.logger
                    .error(decl.location().behind(), "missed a semicolon?");
            }
        }
        PDecl::Fn(PFnDecl {
            keyword,
            name_opt,
            param_list_opt,
            arrow_opt,
            result_opt,
            block_opt,
        }) => {
            if name_opt.is_none() {
                vx.logger
                    .error(keyword.location().clone(), "missed the function name?");
            }

            match param_list_opt {
                Some(param_list) => validate_param_list(param_list, vx),
                None => vx
                    .logger
                    .error(keyword.location().clone(), "missed param list?"),
            }

            validate_result(arrow_opt.as_ref(), result_opt.as_ref(), vx);

            match block_opt {
                Some(block) => validate_block(block, vx),
                None => vx
                    .logger
                    .error(keyword.location().clone(), "missed the body?"),
            }
        }
        PDecl::ExternFn(PExternFnDecl {
            extern_keyword,
            fn_keyword,
            name_opt,
            param_list_opt,
            arrow_opt,
            result_opt,
            semi_opt,
        }) => {
            let location = extern_keyword
                .location()
                .clone()
                .unite(fn_keyword.location());

            if name_opt.is_none() {
                vx.logger
                    .error(location.clone(), "missed the function name?");
            }

            match param_list_opt {
                Some(param_list) => validate_param_list(param_list, vx),
                None => vx.logger.error(location.clone(), "missed param list?"),
            }

            validate_result(arrow_opt.as_ref(), result_opt.as_ref(), vx);

            if semi_required && semi_opt.is_none() {
                vx.logger
                    .error(decl.location().behind(), "missed a semicolon?");
            }
        }
    }
}

fn validate_root(root: &PRoot, vx: &Vx) {
    const SEMI_REQUIRED: bool = true;

    for decl in &root.decls {
        validate_decl(decl, vx, Placement::Global, SEMI_REQUIRED);
    }
}

pub(crate) fn validate_syntax(root: &PRoot, logger: Logger) {
    let vx = Vx::new(logger);
    validate_root(root, &vx);
}
