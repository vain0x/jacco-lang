use super::*;
use std::rc::Rc;

enum IsRequired {
    True(Location),
    False,
}

struct Vx {
    tokens: Rc<PTokens>,
    logger: Logger,
}

impl Vx {
    fn new(tokens: Rc<PTokens>, logger: Logger) -> Self {
        Vx { tokens, logger }
    }

    fn tokens(&self) -> &PTokens {
        &self.tokens
    }
}

fn error_node(have_location: impl HaveLocation, message: impl Into<String>, vx: &Vx) {
    vx.logger.error(&have_location.location(), message);
}

fn error_behind_node(have_location: impl HaveLocation, message: impl Into<String>, vx: &Vx) {
    vx.logger.error(&have_location.location().behind(), message);
}

fn error_token(token: PToken, message: impl Into<String>, vx: &Vx) {
    let location = token.location(&vx.tokens);
    vx.logger.error(location, message);
}

fn error_behind_token(token: PToken, message: impl Into<String>, vx: &Vx) {
    let location = token.location(vx.tokens());
    vx.logger.error(location, message)
}

fn validate_brace_matching(left: PToken, right_opt: Option<PToken>, vx: &Vx) {
    if right_opt.is_none() {
        error_token(left, "maybe missed a right brace?", vx);
    }
}

fn validate_param(param: &PParam, vx: &Vx) {
    match (&param.colon_opt, &param.ty_opt) {
        (Some(_), Some(ty)) => validate_ty(&ty, vx),
        _ => error_behind_node(param, "maybe missed type ascription?", vx),
    }
}

fn validate_param_list(param_list: &PParamList, vx: &Vx) {
    for (i, param) in param_list.params.iter().enumerate() {
        validate_param(param, vx);

        let is_last = i + 1 == param_list.params.len();
        if !is_last && param.comma_opt.is_none() {
            error_node(param, "maybe missing following comma?", vx);
        }
    }

    if param_list.right_paren_opt.is_none() {
        error_node(param_list, "maybe missing closing paren?", vx);
    }
}

fn validate_result(arrow_opt: Option<PToken>, ty_opt: Option<&PTy>, vx: &Vx) {
    match (arrow_opt, ty_opt) {
        (Some(_), Some(ty)) => validate_ty(ty, vx),
        (Some(arrow), None) => error_token(arrow, "missed the type of result?", vx),
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
            error_node(arg, "maybe missed following comma?", vx);
        }
    }

    if arg_list.right_paren_opt.is_none() {
        error_node(arg_list, "maybe missed closing paren?", vx);
    }
}

fn validate_ty(ty: &PTy, vx: &Vx) {
    match ty {
        PTy::Name(_) | PTy::Never { .. } => {}
        PTy::Unit(PUnitTy {
            right_paren_opt, ..
        }) => {
            if right_paren_opt.is_none() {
                error_node(ty, "maybe missed closing paren?", vx);
            }
        }
        PTy::Ptr(PPtrTy { ty_opt, .. }) => {
            if ty_opt.is_none() {
                error_node(ty, "maybe missed following type?", vx);
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
            validate_brace_matching(*left_brace, *right_brace_opt, vx);
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

    validate_brace_matching(block.left_brace, block.right_brace_opt, vx);
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
            validate_brace_matching(*left_brace, *right_brace_opt, vx);

            for (i, field) in fields.iter().enumerate() {
                match &field.colon_opt {
                    Some(colon) => validate_expr_opt(
                        field.value_opt.as_ref(),
                        IsRequired::True(colon.location(vx.tokens())),
                        vx,
                    ),
                    None => error_behind_node(&field.name, "missed a colon?", vx),
                }

                let comma_is_required = {
                    let is_last = i + 1 == fields.len();
                    !is_last
                };
                if comma_is_required && field.comma_opt.is_none() {
                    error_behind_node(field, "missed a comma?", vx);
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
                error_behind_token(*dot, "missed field name?", vx);
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
                error_token(*keyword, "maybe missed a type?", vx);
            }
        }
        PExpr::UnaryOp(PUnaryOpExpr {
            arg_opt, location, ..
        }) => match arg_opt.as_deref() {
            Some(arg) => validate_expr(arg, vx),
            None => error_node(
                location,
                "maybe missed the argument of the unary operator?",
                vx,
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
                None => error_node(location, "maybe missed the right-hand side?", vx),
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
                _ => error_token(*pipe, "expected call expr here", vx),
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
            cond_opt,
            body_opt,
            else_opt,
            alt_opt,
        }) => {
            match cond_opt {
                Some(cond) => validate_expr(cond, vx),
                None => error_behind_token(*keyword, "maybe missed an expression?", vx),
            }

            match body_opt {
                Some(body) => validate_block(body, vx),
                None => error_token(*keyword, "maybe missed body of the if expression?", vx),
            }

            match (else_opt, alt_opt) {
                (Some(_), Some(alt)) => validate_expr(alt, vx),
                (Some(else_keyword), None) => error_behind_token(
                    *else_keyword,
                    "maybe missed the body of the else clause?",
                    vx,
                ),
                _ => {}
            }
        }
        PExpr::Match(PMatchExpr {
            keyword,
            cond_opt,
            left_brace_opt,
            arms,
            right_brace_opt,
        }) => {
            match cond_opt {
                Some(cond) => validate_expr(cond, vx),
                None => error_behind_token(*keyword, "maybe missed an expression?", vx),
            }

            for (i, arm) in arms.iter().enumerate() {
                validate_pat(&arm.pat, vx);

                match &arm.arrow_opt {
                    Some(arrow) => validate_expr_opt(
                        arm.body_opt.as_deref(),
                        IsRequired::True(arrow.location(vx.tokens())),
                        vx,
                    ),
                    None => error_behind_node(&arm.pat, "maybe missed an => arrow here?", vx),
                }

                if i + 1 != arms.len() && arm.comma_opt.is_none() {
                    error_behind_node(arm, "maybe missed a comma?", vx);
                }
            }

            match (left_brace_opt, right_brace_opt) {
                (Some(_), Some(_)) => {}
                (Some(left_brace), None) => {
                    error_token(*left_brace, "maybe missed right brace?", vx)
                }
                _ => error_token(*keyword, "maybe missed body of match?", vx),
            }
        }
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt,
            body_opt,
            ..
        }) => {
            match cond_opt {
                Some(cond) => validate_expr(cond, vx),
                None => error_behind_token(*keyword, "maybe missed an expression?", vx),
            }

            match body_opt {
                Some(body) => validate_block(body, vx),
                None => {
                    error_behind_node(expr, "maybe missed the body of the while expression?", vx)
                }
            }
        }
        PExpr::Loop(PLoopExpr {
            keyword, body_opt, ..
        }) => match body_opt {
            Some(body) => validate_block(body, vx),
            None => error_behind_token(*keyword, "maybe missed body of the if expression?", vx),
        },
    }
}

fn validate_expr_opt(expr_opt: Option<&PExpr>, is_required: IsRequired, vx: &Vx) {
    match (expr_opt, is_required) {
        (Some(expr), _) => validate_expr(expr, vx),
        (None, IsRequired::True(location)) => {
            error_node(location, "maybe missed an expression?", vx)
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
                    IsRequired::True(equal.location(vx.tokens())),
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
            validate_brace_matching(*left_brace, *right_brace_opt, vx);

            for (i, field) in fields.iter().enumerate() {
                if field.colon_opt.is_none() {
                    error_behind_node(&field.name, "maybe missed a colon?", vx);
                }

                if field.ty_opt.is_none() {
                    error_node(field, "maybe missed a type?", vx);
                }

                let comma_is_required = {
                    let is_last = i + 1 == fields.len();
                    !is_last
                };
                if comma_is_required && field.comma_opt.is_none() {
                    error_behind_node(field, "maybe missed a comma?", vx);
                }
            }
        }
    }
}

fn validate_decl(decl: &PDecl, vx: &Vx, placement: Placement, semi_required: bool) {
    match (decl, placement) {
        (PDecl::Expr { .. }, Placement::Global) | (PDecl::Let { .. }, Placement::Global) => {
            error_node(decl, "not allowed", vx);
        }
        _ => {}
    }

    match decl {
        PDecl::Expr(PExprDecl { expr, semi_opt }) => {
            validate_expr(expr, vx);

            if semi_required && semi_opt.is_none() && !expr.ends_with_block() {
                error_behind_node(decl, "missed a semicolon?", vx);
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
                error_behind_token(*keyword, "missed variable name?", vx);
            }

            validate_ty_opt(ty_opt.as_ref(), vx);

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                (Some(equal), None) => error_behind_token(*equal, "missed an expression?", vx),
                _ => {}
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                error_behind_node(&decl, "missed a semicolon?", vx);
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
                error_token(*keyword, "missed constant name?", vx);
            }

            match (colon_opt, ty_opt) {
                (Some(_), Some(ty)) => validate_ty(ty, vx),
                _ => error_token(
                    *keyword,
                    "maybe missed type of constant? (hint: that's required unlike let)",
                    vx,
                ),
            }

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                _ => error_token(*keyword, "maybe missed value of constant?", vx),
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                error_behind_node(decl, "missed a semicolon?", vx);
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
                error_token(*keyword, "missed static var name?", vx);
            }

            match (colon_opt, ty_opt) {
                (Some(_), Some(ty)) => validate_ty(ty, vx),
                _ => error_token(
                    *keyword,
                    "maybe missed type of static var? (hint: that's required unlike let)",
                    vx,
                ),
            }

            match (equal_opt, init_opt) {
                (Some(_), Some(init)) => validate_expr(init, vx),
                (Some(_), None) => error_token(*keyword, "maybe missed value of static var?", vx),
                (None, _) => {}
            }

            if semi_required
                && semi_opt.is_none()
                && !init_opt
                    .as_ref()
                    .map_or(false, |init| init.ends_with_block())
            {
                error_behind_node(decl, "missed a semicolon?", vx);
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
                error_token(*keyword, "missed the function name?", vx);
            }

            match param_list_opt {
                Some(param_list) => validate_param_list(param_list, vx),
                None => error_token(*keyword, "missed param list?", vx),
            }

            validate_result(*arrow_opt, result_ty_opt.as_ref(), vx);

            match block_opt {
                Some(block) => validate_block(block, vx),
                None => error_token(*keyword, "missed the body?", vx),
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
                .location(vx.tokens())
                .unite(fn_keyword.location(vx.tokens()));

            if name_opt.is_none() {
                error_behind_node(location, "missed the function name?", vx);
            }

            match param_list_opt {
                Some(param_list) => validate_param_list(param_list, vx),
                None => error_node(location, "missed param list?", vx),
            }

            validate_result(*arrow_opt, result_ty_opt.as_ref(), vx);

            if semi_required && semi_opt.is_none() {
                error_behind_node(decl, "missed a semicolon?", vx);
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
                error_token(*keyword, "missed enum name?", vx);
            }

            for variant in variants {
                validate_variant(variant, vx);
            }

            match left_brace_opt {
                Some(left_brace) => validate_brace_matching(*left_brace, *right_brace_opt, vx),
                None => error_token(*keyword, "maybe missed left brace?", vx),
            }
        }
        PDecl::Struct(PStructDecl {
            keyword,
            variant_opt,
            semi_opt,
        }) => {
            match variant_opt {
                Some(variant) => validate_variant(variant, vx),
                None => error_token(*keyword, "maybe missed struct name?", vx),
            }

            let ends_with_block = || variant_opt.iter().all(|variant| variant.ends_with_block());
            if semi_required && semi_opt.is_none() && !ends_with_block() {
                error_behind_node(decl, "maybe missed a semicolon?", vx);
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
        error_token(*token, "this token is ignored", vx);
    }
}

pub(crate) fn validate_syntax(root: &PRoot, logger: Logger) {
    // FIXME: clone しない
    let tokens = Rc::new(root.tokens.clone());
    let vx = Vx::new(tokens, logger);
    validate_root(root, &vx);
}
