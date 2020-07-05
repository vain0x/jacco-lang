use super::*;

enum IsRequired<'a> {
    True(&'a Location),
    False,
}

struct Vx {
    logger: Logger,
}

impl Vx {
    fn new(logger: Logger) -> Self {
        Vx { logger }
    }
}

fn validate_brace_matching(left: &TokenData, right_opt: Option<&TokenData>, vx: &Vx) {
    if right_opt.is_none() {
        vx.logger.error(left, "maybe missed a right brace?");
    }
}

fn validate_param(param: &PParam, vx: &Vx) {
    match (&param.colon_opt, &param.ty_opt) {
        (Some(_), Some(ty)) => validate_ty(&ty, vx),
        _ => vx.logger.error(param, "maybe missed type ascription?"),
    }
}

fn validate_param_list(param_list: &PParamList, vx: &Vx) {
    for (i, param) in param_list.params.iter().enumerate() {
        validate_param(param, vx);

        let is_last = i + 1 == param_list.params.len();
        if !is_last && param.comma_opt.is_none() {
            vx.logger.error(param, "maybe missing following comma?");
        }
    }

    if param_list.right_paren_opt.is_none() {
        vx.logger.error(param_list, "maybe missing closing paren?");
    }
}

fn validate_result(arrow_opt: Option<&TokenData>, ty_opt: Option<&PTy>, vx: &Vx) {
    match (arrow_opt, ty_opt) {
        (Some(_), Some(ty)) => validate_ty(ty, vx),
        (Some(arrow), None) => vx.logger.error(arrow, "missed the type of result?"),
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
            vx.logger.error(arg, "maybe missed following comma?");
        }
    }

    if arg_list.right_paren_opt.is_none() {
        vx.logger.error(arg_list, "maybe missed closing paren?");
    }
}

fn validate_ty(ty: &PTy, vx: &Vx) {
    match ty {
        PTy::Name(_) | PTy::Never { .. } => {}
        PTy::Unit(PUnitTy {
            right_paren_opt, ..
        }) => {
            if right_paren_opt.is_none() {
                vx.logger.error(ty, "maybe missed closing paren?");
            }
        }
        PTy::Ptr(PPtrTy { ty_opt, .. }) => {
            if ty_opt.is_none() {
                vx.logger.error(ty, "maybe missed following type?");
            }
        }
    }
}

fn validate_ty_opt(ty_opt: Option<&PTy>, vx: &Vx) {
    if let Some(ty) = ty_opt {
        validate_ty(ty, vx)
    }
}

fn validate_pat(pat: &PPat, vx: &Vx) {
    match pat {
        PPat::Name(_) => {}
        PPat::Record(PRecordPat {
            left_brace,
            right_brace_opt,
            ..
        }) => {
            validate_brace_matching(left_brace, right_brace_opt.as_ref(), vx);
        }
    }
}

fn validate_cond(
    left_paren_opt: Option<&TokenData>,
    cond_opt: Option<&PExpr>,
    right_paren_opt: Option<&TokenData>,
    get_location: impl Fn() -> Location,
    vx: &Vx,
) {
    match (left_paren_opt, cond_opt, right_paren_opt) {
        (Some(_), Some(cond), Some(_)) => {
            validate_expr(cond, vx);
        }
        (None, None, None) | (Some(_), None, Some(_)) => {
            vx.logger.error(&get_location(), "maybe missed condition?");
        }
        (None, Some(cond), None) => {
            vx.logger.error(
                cond,
                "maybe missed a pair of parenthesis around the condition?",
            );
            validate_expr(cond, vx);
        }
        (Some(_), Some(cond), None) => {
            vx.logger
                .error(&cond.location().behind(), "maybe missed a right paren?");
            validate_expr(cond, vx);
        }
        (None, Some(cond), Some(_)) => {
            vx.logger.error(
                &cond.location().ahead(),
                "maybe missed a left paren in front of the condition?",
            );
            validate_expr(cond, vx);
        }
        (None, None, Some(paren)) => {
            vx.logger.error(
                &paren.location().ahead(),
                "maybe missed a left paren and condition?",
            );
        }
        (Some(paren), None, None) => {
            vx.logger.error(
                &paren.location().behind(),
                "maybe missed a condition and a right paren?",
            );
        }
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

    validate_brace_matching(&block.left_brace, block.right_brace_opt.as_ref(), vx);
}

fn validate_expr(expr: &PExpr, vx: &Vx) {
    match expr {
        PExpr::Int(_) | PExpr::Float(_) | PExpr::Char(_) | PExpr::Str(_) => {
            // FIXME: should verify
        }
        PExpr::True(_) | PExpr::False(_) => {}
        PExpr::Name(_) => {}
        PExpr::Record(PRecordExpr {
            name: _,
            left_brace,
            fields,
            right_brace_opt,
        }) => {
            validate_brace_matching(&left_brace, right_brace_opt.as_ref(), vx);

            for (i, field) in fields.iter().enumerate() {
                match &field.colon_opt {
                    Some(colon) => validate_expr_opt(
                        field.value_opt.as_ref(),
                        IsRequired::True(colon.as_location()),
                        vx,
                    ),
                    None => vx
                        .logger
                        .error(&field.name.location().behind(), "missed a colon?"),
                }

                let comma_is_required = {
                    let is_last = i + 1 == fields.len();
                    !is_last
                };
                if comma_is_required && field.comma_opt.is_none() {
                    vx.logger
                        .error(&field.location().behind(), "missed a comma?");
                }
            }
        }
        PExpr::Tuple(PTupleExpr { arg_list }) => validate_arg_list(arg_list, vx),
        PExpr::DotField(PDotFieldExpr {
            left,
            dot,
            name_opt,
        }) => {
            validate_expr(&left, vx);

            if name_opt.is_none() {
                vx.logger
                    .error(&dot.location().behind(), "missed field name?");
            }
        }
        PExpr::Call(PCallExpr { left, arg_list }) | PExpr::Index(PIndexExpr { left, arg_list }) => {
            validate_expr(&left, vx);
            validate_arg_list(arg_list, vx);
        }
        PExpr::As(PAsExpr {
            left,
            keyword,
            ty_opt,
        }) => {
            validate_expr(&left, vx);
            validate_ty_opt(ty_opt.as_ref(), vx);

            if ty_opt.is_none() {
                vx.logger.error(keyword, "maybe missed a type?");
            }
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            arg_opt, location, ..
        }) => match arg_opt.as_deref() {
            Some(arg) => validate_expr(arg, vx),
            None => vx
                .logger
                .error(location, "maybe missed the argument of the unary operator?"),
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
                    .error(location, "maybe missed the right-hand side?"),
            }
        }
        PExpr::Pipe(PPipeExpr {
            left,
            pipe,
            right_opt,
        }) => {
            validate_expr(left, vx);

            match right_opt.as_deref() {
                Some(right @ PExpr::Call(_)) => validate_expr(right, vx),
                _ => vx.logger.error(pipe, "expected call expr here"),
            }
        }
        PExpr::Block(PBlockExpr(block)) => {
            validate_block(block, vx);
        }
        PExpr::Break(PBreakExpr { arg_opt, .. }) => {
            validate_expr_opt(arg_opt.as_deref(), IsRequired::False, vx);
        }
        PExpr::Continue { .. } => {}
        PExpr::Return(PReturnExpr { arg_opt, .. }) => {
            validate_expr_opt(arg_opt.as_deref(), IsRequired::False, vx);
        }
        PExpr::If(PIfExpr {
            keyword,
            left_paren_opt,
            cond_opt,
            right_paren_opt,
            body_opt,
            else_opt,
            alt_opt,
        }) => {
            validate_cond(
                left_paren_opt.as_ref(),
                cond_opt.as_deref(),
                right_paren_opt.as_ref(),
                || keyword.location().clone(),
                vx,
            );

            match body_opt {
                Some(body) => validate_block(body, vx),
                None => vx
                    .logger
                    .error(keyword, "maybe missed body of the if expression?"),
            }

            match (else_opt, alt_opt) {
                (Some(_), Some(alt)) => validate_expr(alt, vx),
                (Some(else_keyword), None) => vx
                    .logger
                    .error(else_keyword, "maybe missed the body of the else clause?"),
                (None, Some(_)) => {
                    // unreachable
                }
                (None, None) => {}
            }
        }
        PExpr::Match(PMatchExpr {
            keyword,
            left_paren_opt,
            cond_opt,
            right_paren_opt,
            left_brace_opt,
            arms,
            right_brace_opt,
        }) => {
            validate_cond(
                left_paren_opt.as_ref(),
                cond_opt.as_deref(),
                right_paren_opt.as_ref(),
                || keyword.location().clone(),
                vx,
            );

            for (i, arm) in arms.iter().enumerate() {
                validate_pat(&arm.pat, vx);

                match &arm.arrow_opt {
                    Some(arrow) => validate_expr_opt(
                        arm.body_opt.as_deref(),
                        IsRequired::True(arrow.as_location()),
                        vx,
                    ),
                    None => vx.logger.error(
                        &arm.pat.location().behind(),
                        "maybe missed an => arrow here?",
                    ),
                }

                if i + 1 != arms.len() && arm.comma_opt.is_none() {
                    vx.logger
                        .error(&arm.location().behind(), "maybe missed a comma?");
                }
            }

            match (left_brace_opt, right_brace_opt) {
                (Some(_), Some(_)) => {}
                (Some(left_brace), None) => {
                    vx.logger.error(left_brace, "maybe missed right brace?")
                }
                _ => vx.logger.error(keyword, "maybe missed body of match?"),
            }
        }
        PExpr::While(PWhileExpr {
            keyword,
            left_paren_opt,
            cond_opt,
            right_paren_opt,
            body_opt,
            ..
        }) => {
            validate_cond(
                left_paren_opt.as_ref(),
                cond_opt.as_deref(),
                right_paren_opt.as_ref(),
                || keyword.location().clone(),
                vx,
            );

            match body_opt {
                Some(body) => validate_block(body, vx),
                None => vx
                    .logger
                    .error(keyword, "maybe missed the body of the while expression?"),
            }
        }
        PExpr::Loop(PLoopExpr {
            keyword, body_opt, ..
        }) => match body_opt {
            Some(body) => validate_block(body, vx),
            None => vx
                .logger
                .error(keyword, "maybe missed body of the if expression?"),
        },
    }
}

fn validate_expr_opt(expr_opt: Option<&PExpr>, is_required: IsRequired, vx: &Vx) {
    match (expr_opt, is_required) {
        (Some(expr), _) => validate_expr(expr, vx),
        (None, IsRequired::True(location)) => {
            vx.logger.error(location, "maybe missed an expression?")
        }
        (None, IsRequired::False) => {}
    }
}

fn validate_variant(variant: &PVariantDecl, vx: &Vx) {
    match variant {
        PVariantDecl::Const(PConstVariantDecl {
            equal_opt,
            value_opt,
            ..
        }) => {
            if let Some(equal) = equal_opt {
                validate_expr_opt(
                    value_opt.as_deref(),
                    IsRequired::True(equal.as_location()),
                    vx,
                );
            }
        }
        PVariantDecl::Record(PRecordVariantDecl {
            name: _,
            left_brace,
            fields,
            right_brace_opt,
            comma_opt: _,
        }) => {
            validate_brace_matching(&left_brace, right_brace_opt.as_ref(), vx);

            for (i, field) in fields.iter().enumerate() {
                if field.colon_opt.is_none() {
                    vx.logger
                        .error(&field.name.location().behind(), "maybe missed a colon?");
                }

                if field.ty_opt.is_none() {
                    vx.logger.error(field, "maybe missed a type?");
                }

                let comma_is_required = {
                    let is_last = i + 1 == fields.len();
                    !is_last
                };
                if comma_is_required && field.comma_opt.is_none() {
                    vx.logger
                        .error(&field.location().behind(), "maybe missed a comma?");
                }
            }
        }
    }
}

fn validate_decl(decl: &PDecl, vx: &Vx, placement: Placement, semi_required: bool) {
    match (decl, placement) {
        (PDecl::Expr { .. }, Placement::Global) | (PDecl::Let { .. }, Placement::Global) => {
            vx.logger.error(decl, "not allowed");
        }
        _ => {}
    }

    match decl {
        PDecl::Expr(PExprDecl { expr, semi_opt }) => {
            validate_expr(expr, vx);

            if semi_required && semi_opt.is_none() && !expr.ends_with_block() {
                vx.logger
                    .error(&decl.location().behind(), "missed a semicolon?");
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
                vx.logger.error(keyword, "missed variable name?");
            }

            validate_ty_opt(ty_opt.as_ref(), vx);

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                (Some(equal), None) => vx.logger.error(equal, "missed an expression?"),
                (None, Some(_)) | (None, None) => {}
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                vx.logger
                    .error(&decl.location().behind(), "missed a semicolon?");
            }
        }
        PDecl::Const(PConstDecl {
            keyword,
            name_opt,
            colon_opt,
            ty_opt,
            equal_opt,
            init_opt,
            semi_opt,
        }) => {
            if name_opt.is_none() {
                vx.logger.error(keyword, "missed constant name?");
            }

            match (colon_opt, ty_opt) {
                (Some(_), Some(ty)) => validate_ty(ty, vx),
                _ => vx.logger.error(
                    keyword,
                    "maybe missed type of constant? (hint: that's required unlike let)",
                ),
            }

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                _ => vx.logger.error(keyword, "maybe missed value of constant?"),
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                vx.logger
                    .error(&decl.location().behind(), "missed a semicolon?");
            }
        }
        PDecl::Static(PStaticDecl {
            keyword,
            name_opt,
            colon_opt,
            ty_opt,
            equal_opt,
            init_opt,
            semi_opt,
        }) => {
            if name_opt.is_none() {
                vx.logger.error(keyword, "missed static var name?");
            }

            match (colon_opt, ty_opt) {
                (Some(_), Some(ty)) => validate_ty(ty, vx),
                _ => vx.logger.error(
                    keyword,
                    "maybe missed type of static var? (hint: that's required unlike let)",
                ),
            }

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                (Some(_), None) => vx
                    .logger
                    .error(keyword, "maybe missed value of static var?"),
                (None, _) => {}
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                vx.logger
                    .error(&decl.location().behind(), "missed a semicolon?");
            }
        }
        PDecl::Fn(PFnDecl {
            keyword,
            name_opt,
            param_list_opt,
            arrow_opt,
            result_ty_opt,
            block_opt,
            ..
        }) => {
            if name_opt.is_none() {
                vx.logger.error(keyword, "missed the function name?");
            }

            match param_list_opt {
                Some(param_list) => validate_param_list(param_list, vx),
                None => vx.logger.error(keyword, "missed param list?"),
            }

            validate_result(arrow_opt.as_ref(), result_ty_opt.as_ref(), vx);

            match block_opt {
                Some(block) => validate_block(block, vx),
                None => vx.logger.error(keyword, "missed the body?"),
            }
        }
        PDecl::ExternFn(PExternFnDecl {
            extern_keyword,
            fn_keyword,
            name_opt,
            param_list_opt,
            arrow_opt,
            result_ty_opt,
            semi_opt,
            ..
        }) => {
            let location = extern_keyword
                .location()
                .clone()
                .unite(&fn_keyword.location());

            if name_opt.is_none() {
                vx.logger.error(&location, "missed the function name?");
            }

            match param_list_opt {
                Some(param_list) => validate_param_list(param_list, vx),
                None => vx.logger.error(&location, "missed param list?"),
            }

            validate_result(arrow_opt.as_ref(), result_ty_opt.as_ref(), vx);

            if semi_required && semi_opt.is_none() {
                vx.logger
                    .error(&decl.location().behind(), "missed a semicolon?");
            }
        }
        PDecl::Enum(PEnumDecl {
            // FIXME: 可視性を処理する
            vis_opt: _,
            keyword,
            name_opt,
            left_brace_opt,
            variants,
            right_brace_opt,
        }) => {
            if name_opt.is_none() {
                vx.logger.error(keyword, "missed enum name?");
            }

            for variant in variants {
                validate_variant(variant, vx);
            }

            match left_brace_opt {
                Some(left_brace) => {
                    validate_brace_matching(&left_brace, right_brace_opt.as_ref(), vx)
                }
                None => vx.logger.error(keyword, "maybe missed left brace?"),
            }
        }
        PDecl::Struct(PStructDecl {
            keyword,
            variant_opt,
            semi_opt,
        }) => {
            match variant_opt {
                Some(variant) => validate_variant(variant, vx),
                None => vx.logger.error(keyword, "maybe missed struct name?"),
            }

            let ends_with_block = || variant_opt.iter().all(|variant| variant.ends_with_block());
            if semi_required && semi_opt.is_none() && !ends_with_block() {
                vx.logger
                    .error(&decl.location().behind(), "maybe missed a semicolon?");
            }
        }
    }
}

fn validate_root(root: &PRoot, vx: &Vx) {
    const SEMI_REQUIRED: bool = true;

    for decl in &root.decls {
        validate_decl(decl, vx, Placement::Global, SEMI_REQUIRED);
    }

    for token in &root.skipped {
        vx.logger.error(token, "this token is ignored");
    }
}

pub(crate) fn validate_syntax(root: &PRoot, logger: Logger) {
    let vx = Vx::new(logger);
    validate_root(root, &vx);
}
