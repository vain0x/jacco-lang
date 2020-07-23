use super::*;

pub(crate) type AfterQualifiableName = (PName, AName, ParseEnd);
pub(crate) type AfterUnqualifiableName = (PName, AName, ParseEnd);
pub(crate) type AfterParam = (PParam, AParamDecl, ParseEnd);
pub(crate) type AfterParamList = (PParamList, Vec<AParamDecl>);
pub(crate) type AfterArg = (PArg, AExpr, ParseEnd);
pub(crate) type AfterArgList = (PArgList, Vec<AExpr>);
pub(crate) type AfterTy = (PTy, ATy, TyEnd);
pub(crate) type AfterPat = (PPat, APat, PatEnd);
pub(crate) type AfterFieldExpr = (PFieldExpr, AFieldExpr, ParseEnd);
pub(crate) type AfterArm = (PArm, AArm, ParseEnd);
pub(crate) type AfterExpr = (PExpr, AExpr, ExprEnd);
pub(crate) type AfterBlock = (PBlock, Vec<ADecl>, ParseEnd);
pub(crate) type AfterDeclModifiers = (DeclStart, Option<PVis>);
pub(crate) type AfterVariantDecl = (PVariantDecl, AVariantDecl, ParseEnd);
pub(crate) type AfterVariantDecls = (Vec<PVariantDecl>, Vec<AVariantDecl>);
pub(crate) type AfterFieldDecl = (PFieldDecl, AFieldLikeDecl, ParseEnd);
pub(crate) type AfterFieldDecls = (Vec<PFieldDecl>, Vec<AFieldLikeDecl>);
pub(crate) type AfterDecl = (PDecl, ADecl, DeclEnd);
pub(crate) type AfterSemi = (Vec<PDecl>, Option<PExpr>, Vec<ADecl>);
pub(crate) type AfterRoot = Vec<AfterDecl>;

fn decompose_opt<P, A, E>(opt: Option<(P, A, E)>) -> (Option<P>, Option<A>) {
    match opt {
        Some((p, a, _)) => (Some(p), Some(a)),
        None => (None, None),
    }
}

// -----------------------------------------------
// 名前
// -----------------------------------------------

pub(crate) fn alloc_name(
    event: ParseStart,
    quals: Vec<PNameQual>,
    token: PToken,
    px: &mut Px,
) -> AfterQualifiableName {
    let text = token.text(px.tokens()).to_string();
    let full_name = {
        let mut s = String::new();

        for qual in &quals {
            s += qual.name.text(px.tokens());
            s += "::";
        }

        s += token.text(px.tokens());
        s
    };

    let p_name = px.names.alloc(PNameData {
        quals,
        token,
        text,
        full_name,
    });
    (p_name, AName, event.end(PElementKind::Name, px))
}

// -----------------------------------------------
// パラメータ
// -----------------------------------------------

pub(crate) fn alloc_param(
    event: ParseStart,
    name: AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    ty_opt: Option<AfterTy>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterParam {
    let (name, a_name, _) = name;
    let (ty_opt, a_ty_opt) = decompose_opt(ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));

    (
        PParam {
            name,
            colon_opt,
            ty_opt,
            comma_opt,
        },
        AParamDecl {
            name: a_name,
            ty_opt: a_ty_opt,
        },
        event.end(PElementKind::ParamDecl, px),
    )
}

pub(crate) fn alloc_param_list(
    left_paren: PToken,
    params: Vec<AfterParam>,
    right_paren_opt: Option<PToken>,
    _px: &mut Px,
) -> AfterParamList {
    let (params, param_decls) = params
        .into_iter()
        .map(|(param, param_decl, _)| (param, param_decl))
        .unzip();

    (
        PParamList {
            left_paren,
            params,
            right_paren_opt,
        },
        param_decls,
    )
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) fn alloc_name_ty(event: TyStart, name: AfterQualifiableName, px: &mut Px) -> AfterTy {
    let (name, a_name, _) = name;
    (
        PTy::Name(name),
        ATy::Name(a_name),
        event.end(PElementKind::NameTy, px),
    )
}

pub(crate) fn alloc_unit_ty(
    event: TyStart,
    left_paren: PToken,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterTy {
    (
        PTy::Unit(PUnitTy {
            left_paren,
            right_paren_opt,
        }),
        ATy::Unit,
        event.end(PElementKind::UnitTy, px),
    )
}

pub(crate) fn alloc_never_ty(event: TyStart, bang: PToken, px: &mut Px) -> AfterTy {
    (
        PTy::Never(PNeverTy { bang }),
        ATy::Never,
        event.end(PElementKind::NeverTy, px),
    )
}

pub(crate) fn alloc_ptr_ty(
    event: TyStart,
    rep: OneOrTwo,
    star: PToken,
    mut_opt: Option<PMut>,
    ty_opt: Option<AfterTy>,
    px: &mut Px,
) -> AfterTy {
    let (ty_opt, a_ty_opt) = match ty_opt {
        Some((ty, a_ty, _)) => ((Some(Box::new(ty)), Some(a_ty))),
        None => (None, None),
    };
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));

    (
        PTy::Ptr(PPtrTy {
            star,
            mut_opt,
            ty_opt,
            rep,
        }),
        ATy::Ptr(APtrTy {
            mut_opt: mut_opt.map(|p| p.0),
            ty_opt: a_ty_opt,
        }),
        event.end(PElementKind::PtrTy, px),
    )
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

pub(crate) fn alloc_char_pat(event: PatStart, token: PToken, px: &mut Px) -> AfterPat {
    (
        PPat::Char(token),
        APat::Char(token),
        event.end(PElementKind::CharPat, px),
    )
}

pub(crate) fn alloc_name_pat(event: PatStart, name: AfterQualifiableName, px: &mut Px) -> AfterPat {
    let (name, a_name, _) = name;
    (
        PPat::Name(name),
        APat::Name(a_name),
        event.end(PElementKind::NamePat, px),
    )
}

pub(crate) fn alloc_record_pat(
    event: PatStart,
    name: AfterQualifiableName,
    left_brace: PToken,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterPat {
    let (name, a_name, _) = name;
    (
        PPat::Record(PRecordPat {
            name,
            left_brace,
            right_brace_opt,
        }),
        APat::Record(ARecordPat {
            left: a_name,
            fields: vec![],
        }),
        event.end(PElementKind::RecordPat, px),
    )
}

// -----------------------------------------------
// 式
// -----------------------------------------------

pub(crate) fn alloc_unit_expr(
    event: ExprStart,
    left_paren: PToken,
    right_paren: PToken,
    px: &mut Px,
) -> AfterExpr {
    (
        PExpr::Tuple(PTupleExpr {
            arg_list: PArgList {
                left_paren,
                args: vec![],
                right_paren_opt: Some(right_paren),
            },
        }),
        AExpr::Unit,
        event.end(PElementKind::UnitExpr, px),
    )
}

pub(crate) fn alloc_group_expr(
    event: ExprStart,
    left_paren: PToken,
    body_opt: Option<AfterExpr>,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    match body_opt {
        Some((body, a_expr, _)) => (
            PExpr::Tuple(PTupleExpr {
                arg_list: PArgList {
                    left_paren,
                    args: vec![PArg {
                        expr: body,
                        comma_opt: None,
                    }],
                    right_paren_opt,
                },
            }),
            a_expr,
            event.end(PElementKind::GroupExpr, px),
        ),
        None => (
            PExpr::Tuple(PTupleExpr {
                arg_list: PArgList {
                    left_paren,
                    args: vec![],
                    right_paren_opt,
                },
            }),
            AExpr::Unit,
            event.end(PElementKind::UnitExpr, px),
        ),
    }
}

pub(crate) fn alloc_field_expr(
    event: ParseStart,
    name: AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    value_opt: Option<AfterExpr>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterFieldExpr {
    let (name, a_name, _) = name;
    let (value_opt, a_value_opt) = decompose_opt(value_opt);
    let a_value_opt = a_value_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PFieldExpr {
            name,
            colon_opt,
            value_opt,
            comma_opt,
        },
        AFieldExpr {
            field_name: a_name,
            value_opt: a_value_opt,
        },
        event.end(PElementKind::FieldExpr, px),
    )
}

pub(crate) fn alloc_number(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (
        PExpr::Number(PNumberExpr { token }),
        AExpr::Number(token),
        event.end(PElementKind::NumberExpr, px),
    )
}

pub(crate) fn alloc_char(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (
        PExpr::Char(PCharExpr { token }),
        AExpr::Char(token),
        event.end(PElementKind::CharExpr, px),
    )
}

pub(crate) fn alloc_str(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (
        PExpr::Str(PStrExpr { token }),
        AExpr::Str(token),
        event.end(PElementKind::StrExpr, px),
    )
}

pub(crate) fn alloc_true(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (
        PExpr::True(PTrueExpr { token }),
        AExpr::True,
        event.end(PElementKind::TrueExpr, px),
    )
}

pub(crate) fn alloc_false(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (
        PExpr::False(PFalseExpr { token }),
        AExpr::False,
        event.end(PElementKind::FalseExpr, px),
    )
}

pub(crate) fn alloc_name_expr(
    event: ExprStart,
    name: AfterQualifiableName,
    px: &mut Px,
) -> AfterExpr {
    let (name, a_name, _) = name;
    (
        PExpr::Name(name),
        AExpr::Name(a_name),
        event.end(PElementKind::NameExpr, px),
    )
}

pub(crate) fn alloc_record_expr(
    event: ExprStart,
    name: AfterQualifiableName,
    left_brace: PToken,
    fields: Vec<AfterFieldExpr>,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    let (name, a_name, _) = name;
    let (fields, a_fields) = fields
        .into_iter()
        .map(|(p_field_expr, a_field_expr, _)| (p_field_expr, a_field_expr))
        .unzip();
    (
        PExpr::Record(PRecordExpr {
            name,
            left_brace,
            fields,
            right_brace_opt,
        }),
        AExpr::Record(ARecordExpr {
            left: a_name,
            fields: a_fields,
        }),
        event.end(PElementKind::RecordExpr, px),
    )
}

pub(crate) fn alloc_dot_field_expr(
    event: ExprStart,
    left: AfterExpr,
    dot: PToken,
    name_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    let (left, a_left, _) = left;
    let a_left = px.ast.exprs.alloc(a_left);

    (
        PExpr::DotField(PDotFieldExpr {
            left: Box::new(left),
            dot,
            name_opt,
        }),
        AExpr::DotField(ADotFieldExpr {
            left: a_left,
            field_opt: name_opt,
        }),
        event.end(PElementKind::DotFieldExpr, px),
    )
}

pub(crate) fn alloc_call_expr(
    event: ExprStart,
    left: AfterExpr,
    arg_list: AfterArgList,
    px: &mut Px,
) -> AfterExpr {
    let (left, a_left, _) = left;
    let a_left = px.ast.exprs.alloc(a_left);

    let (arg_list, args) = arg_list;
    let args = px.ast.exprs.alloc_slice(args);

    (
        PExpr::Call(PCallExpr {
            left: Box::new(left),
            arg_list,
        }),
        AExpr::Call(ACallLikeExpr { left: a_left, args }),
        event.end(PElementKind::CallExpr, px),
    )
}

pub(crate) fn alloc_index_expr(
    event: ExprStart,
    left: AfterExpr,
    arg_list: AfterArgList,
    px: &mut Px,
) -> AfterExpr {
    let (left, a_left, _) = left;
    let a_left = px.ast.exprs.alloc(a_left);

    let (arg_list, args) = arg_list;
    let args = px.ast.exprs.alloc_slice(args);

    (
        PExpr::Index(PIndexExpr {
            left: Box::new(left),
            arg_list,
        }),
        AExpr::Index(ACallLikeExpr { left: a_left, args }),
        event.end(PElementKind::IndexExpr, px),
    )
}

pub(crate) fn alloc_as_expr(
    event: ExprStart,
    left: AfterExpr,
    keyword: PToken,
    ty_opt: Option<AfterTy>,
    px: &mut Px,
) -> AfterExpr {
    let (left, a_left, _) = left;
    let a_left = px.ast.exprs.alloc(a_left);

    let (ty_opt, a_ty_opt) = decompose_opt(ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));

    (
        PExpr::As(PAsExpr {
            left: Box::new(left),
            keyword,
            ty_opt,
        }),
        AExpr::As(AAsExpr {
            left: a_left,
            ty_opt: a_ty_opt,
        }),
        event.end(PElementKind::AsExpr, px),
    )
}

pub(crate) fn alloc_prefix_expr(
    event: ExprStart,
    unary_op: PUnaryOp,
    token: PToken,
    mut_opt: Option<PMut>,
    arg_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let (arg_opt, a_arg_opt) = decompose_opt(arg_opt);
    let a_arg_opt = a_arg_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PExpr::UnaryOp(PUnaryOpExpr {
            op: unary_op,
            op_token: token,
            mut_opt,
            arg_opt: arg_opt.map(Box::new),
        }),
        AExpr::UnaryOp(AUnaryOpExpr {
            op: unary_op,
            mut_opt: mut_opt.map(|x| x.0),
            arg_opt: a_arg_opt,
        }),
        event.end(PElementKind::UnaryOpExpr, px),
    )
}

pub(crate) fn alloc_binary_op_expr(
    event: ExprStart,
    binary_op: PBinaryOp,
    left: AfterExpr,
    token: PToken,
    right_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let (left, a_left, _) = left;
    let a_left = px.ast.exprs.alloc(a_left);

    let (right_opt, a_right_opt) = decompose_opt(right_opt);
    let a_right_opt = a_right_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PExpr::BinaryOp(PBinaryOpExpr {
            op: binary_op,
            left: Box::new(left),
            op_token: token,
            right_opt: right_opt.map(Box::new),
        }),
        AExpr::BinaryOp(ABinaryOpExpr {
            op: binary_op,
            left: a_left,
            right_opt: a_right_opt,
        }),
        event.end(PElementKind::BinaryOpExpr, px),
    )
}

pub(crate) fn alloc_pipe_expr(
    event: ExprStart,
    left: AfterExpr,
    pipe: PToken,
    right_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let (left, a_left, _) = left;
    let a_left = px.ast.exprs.alloc(a_left);

    let (right_opt, a_right_opt) = decompose_opt(right_opt);
    let a_right_opt = a_right_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PExpr::Pipe(PPipeExpr {
            left: Box::new(left),
            pipe,
            right_opt: right_opt.map(Box::new),
        }),
        AExpr::Pipe(APipeExpr {
            left: a_left,
            right_opt: a_right_opt,
        }),
        event.end(PElementKind::PipeExpr, px),
    )
}

fn do_alloc_block_expr(decls: Vec<ADecl>, px: &mut Px) -> AExpr {
    let decls = px.ast.decls.alloc_slice(decls);
    AExpr::Block(ABlockExpr { decls })
}

pub(crate) fn alloc_block(
    event: ParseStart,
    left_brace: PToken,
    semi: AfterSemi,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterBlock {
    let (decls, last_opt, a_decls) = semi;
    (
        PBlock {
            left_brace,
            decls,
            last_opt: last_opt.map(Box::new),
            right_brace_opt,
        },
        a_decls,
        event.end(PElementKind::BlockExpr, px),
    )
}

pub(crate) fn alloc_block_expr(
    event: ExprStart,
    left_brace: PToken,
    semi: AfterSemi,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    let (decls, last_opt, a_decls) = semi;

    (
        PExpr::Block(PBlockExpr(PBlock {
            left_brace,
            decls,
            last_opt: last_opt.map(Box::new),
            right_brace_opt,
        })),
        do_alloc_block_expr(a_decls, px),
        event.end(PElementKind::BlockExpr, px),
    )
}

pub(crate) fn alloc_break_expr(
    event: ExprStart,
    keyword: PToken,
    arg_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let (arg_opt, a_arg_opt) = decompose_opt(arg_opt);
    let a_arg_opt = a_arg_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PExpr::Break(PBreakExpr {
            keyword,
            arg_opt: arg_opt.map(Box::new),
            loop_id_opt: None,
        }),
        AExpr::Break(AJumpExpr { arg_opt: a_arg_opt }),
        event.end(PElementKind::BreakExpr, px),
    )
}

pub(crate) fn alloc_continue_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    (
        PExpr::Continue(PContinueExpr {
            keyword,
            loop_id_opt: None,
        }),
        AExpr::Continue,
        event.end(PElementKind::ContinueExpr, px),
    )
}

pub(crate) fn alloc_return_expr(
    event: ExprStart,
    keyword: PToken,
    arg_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let (arg_opt, a_arg_opt) = decompose_opt(arg_opt);
    let a_arg_opt = a_arg_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PExpr::Return(PReturnExpr {
            keyword,
            arg_opt: arg_opt.map(Box::new),
            fn_id_opt: None,
        }),
        AExpr::Return(AJumpExpr { arg_opt: a_arg_opt }),
        event.end(PElementKind::ReturnExpr, px),
    )
}

pub(crate) fn alloc_if_expr(
    event: ExprStart,
    keyword: PToken,
    cond_opt: Option<AfterExpr>,
    body_opt: Option<AfterBlock>,
    else_opt: Option<PToken>,
    alt_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let (cond_opt, a_cond_opt) = decompose_opt(cond_opt);
    let a_cond_opt = a_cond_opt.map(|expr| px.ast.exprs.alloc(expr));

    let (body_opt, a_body_opt) = decompose_opt(body_opt);
    let a_body_opt = a_body_opt.map(|decls| {
        let expr = do_alloc_block_expr(decls, px);
        px.ast.exprs.alloc(expr)
    });

    let (alt_opt, a_alt_opt) = decompose_opt(alt_opt);
    let a_alt_opt = a_alt_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PExpr::If(PIfExpr {
            keyword,
            cond_opt: cond_opt.map(Box::new),
            body_opt,
            else_opt,
            alt_opt: alt_opt.map(Box::new),
        }),
        AExpr::If(AIfExpr {
            cond_opt: a_cond_opt,
            body_opt: a_body_opt,
            alt_opt: a_alt_opt,
        }),
        event.end(PElementKind::IfExpr, px),
    )
}

pub(crate) fn alloc_arm(
    event: ParseStart,
    pat: AfterPat,
    arrow_opt: Option<PToken>,
    body_opt: Option<AfterExpr>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterArm {
    let (pat, a_pat, _) = pat;
    let a_pat = px.ast.pats.alloc(a_pat);

    let (body_opt, a_body_opt) = decompose_opt(body_opt);
    let a_body_opt = a_body_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PArm {
            pat,
            arrow_opt,
            body_opt: body_opt.map(Box::new),
            comma_opt,
        },
        AArm {
            pat: a_pat,
            body_opt: a_body_opt,
        },
        event.end(PElementKind::Arm, px),
    )
}

pub(crate) fn alloc_match_expr(
    event: ExprStart,
    keyword: PToken,
    cond_opt: Option<AfterExpr>,
    left_brace_opt: Option<PToken>,
    arms: Vec<AfterArm>,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    let (cond_opt, a_cond_opt) = decompose_opt(cond_opt);
    let a_cond_opt = a_cond_opt.map(|expr| px.ast.exprs.alloc(expr));

    let (arms, a_arms) = arms.into_iter().map(|(a, b, _)| (a, b)).unzip();

    (
        PExpr::Match(PMatchExpr {
            keyword,
            cond_opt: cond_opt.map(Box::new),
            left_brace_opt,
            arms,
            right_brace_opt,
        }),
        AExpr::Match(AMatchExpr {
            cond_opt: a_cond_opt,
            arms: a_arms,
        }),
        event.end(PElementKind::MatchExpr, px),
    )
}

pub(crate) fn alloc_while_expr(
    event: ExprStart,
    keyword: PToken,
    cond_opt: Option<AfterExpr>,
    body_opt: Option<AfterBlock>,
    px: &mut Px,
) -> AfterExpr {
    let (cond_opt, a_cond_opt) = decompose_opt(cond_opt);
    let a_cond_opt = a_cond_opt.map(|expr| px.ast.exprs.alloc(expr));

    let (body_opt, a_body_opt) = decompose_opt(body_opt);
    let a_body_opt = a_body_opt.map(|decls| {
        let expr = do_alloc_block_expr(decls, px);
        px.ast.exprs.alloc(expr)
    });

    (
        PExpr::While(PWhileExpr {
            keyword,
            cond_opt: cond_opt.map(Box::new),
            body_opt,
            loop_id_opt: None,
        }),
        AExpr::While(AWhileExpr {
            cond_opt: a_cond_opt,
            body_opt: a_body_opt,
        }),
        event.end(PElementKind::WhileExpr, px),
    )
}

pub(crate) fn alloc_loop_expr(
    event: ExprStart,
    keyword: PToken,
    body_opt: Option<AfterBlock>,
    px: &mut Px,
) -> AfterExpr {
    let (body_opt, a_body_opt) = decompose_opt(body_opt);
    let a_body_opt = a_body_opt.map(|decls| {
        let expr = do_alloc_block_expr(decls, px);
        px.ast.exprs.alloc(expr)
    });

    (
        PExpr::Loop(PLoopExpr {
            keyword,
            body_opt,
            loop_id_opt: None,
        }),
        AExpr::Loop(ALoopExpr {
            body_opt: a_body_opt,
        }),
        event.end(PElementKind::LoopExpr, px),
    )
}

pub(crate) fn alloc_arg(
    event: ParseStart,
    expr: AfterExpr,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterArg {
    let (expr, a_expr, _) = expr;

    (
        PArg { expr, comma_opt },
        a_expr,
        event.end(PElementKind::Arg, px),
    )
}

pub(crate) fn alloc_arg_list(
    left_paren: PToken,
    args: Vec<AfterArg>,
    right_paren_opt: Option<PToken>,
    _px: &mut Px,
) -> AfterArgList {
    let (args, a_args) = args.into_iter().map(|(arg, a_arg, _)| (arg, a_arg)).unzip();

    (
        PArgList {
            left_paren,
            args,
            right_paren_opt,
        },
        a_args,
    )
}

// -----------------------------------------------
// 宣言
// -----------------------------------------------

fn alloc_modifiers(modifiers: AfterDeclModifiers) -> (DeclStart, ADeclModifiers) {
    let (event, vis_opt) = modifiers;

    (
        event,
        ADeclModifiers {
            vis_opt: vis_opt.map(|(vis, _)| vis),
        },
    )
}

pub(crate) fn alloc_expr_decl(
    event: DeclStart,
    expr: AfterExpr,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (expr, a_expr, _) = expr;
    let a_expr = px.ast.exprs.alloc(a_expr);

    (
        PDecl::Expr(PExprDecl { expr, semi_opt }),
        ADecl::Expr(a_expr),
        event.end(PElementKind::ExprDecl, px),
    )
}

pub(crate) fn alloc_let_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<AfterTy>,
    equal_opt: Option<PToken>,
    init_opt: Option<AfterExpr>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);
    let (ty_opt, a_ty_opt) = decompose_opt(ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));
    let (init_opt, a_init_opt) = decompose_opt(init_opt);
    let a_init_opt = a_init_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PDecl::Let(PLetDecl {
            keyword,
            name_opt,
            colon_opt,
            ty_opt,
            equal_opt,
            init_opt,
            semi_opt,
        }),
        ADecl::Let(AFieldLikeDecl {
            modifiers,
            name_opt: a_name_opt,
            ty_opt: a_ty_opt,
            value_opt: a_init_opt,
        }),
        event.end(PElementKind::LetDecl, px),
    )
}

pub(crate) fn alloc_const_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<AfterTy>,
    equal_opt: Option<PToken>,
    init_opt: Option<AfterExpr>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);
    let (ty_opt, a_ty_opt) = decompose_opt(ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));
    let (init_opt, a_init_opt) = decompose_opt(init_opt);
    let a_init_opt = a_init_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PDecl::Const(PConstDecl {
            keyword,
            name_opt,
            colon_opt,
            ty_opt,
            equal_opt,
            init_opt,
            semi_opt,
        }),
        ADecl::Const(AFieldLikeDecl {
            modifiers,
            name_opt: a_name_opt,
            ty_opt: a_ty_opt,
            value_opt: a_init_opt,
        }),
        event.end(PElementKind::ConstDecl, px),
    )
}

pub(crate) fn alloc_static_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    colon_opt: Option<PToken>,
    ty_opt: Option<AfterTy>,
    equal_opt: Option<PToken>,
    init_opt: Option<AfterExpr>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);
    let (ty_opt, a_ty_opt) = decompose_opt(ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));
    let (init_opt, a_init_opt) = decompose_opt(init_opt);
    let a_init_opt = a_init_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PDecl::Static(PStaticDecl {
            keyword,
            name_opt,
            colon_opt,
            ty_opt,
            equal_opt,
            init_opt,
            semi_opt,
        }),
        ADecl::Static(AFieldLikeDecl {
            modifiers,
            name_opt: a_name_opt,
            ty_opt: a_ty_opt,
            value_opt: a_init_opt,
        }),
        event.end(PElementKind::StaticDecl, px),
    )
}

pub(crate) fn alloc_fn_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    param_list_opt: Option<AfterParamList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<AfterTy>,
    block_opt: Option<AfterBlock>,
    px: &mut Px,
) -> AfterDecl {
    let (_, vis_opt) = modifiers;
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);
    let (param_list_opt, params) = match param_list_opt {
        Some((param_list, params)) => (Some(param_list), params),
        None => (None, vec![]),
    };
    let (result_ty_opt, a_ty_opt) = decompose_opt(result_ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));
    let (block_opt, a_block_opt) = decompose_opt(block_opt);
    let body_opt = a_block_opt.map(|decls| {
        let expr = do_alloc_block_expr(decls, px);
        px.ast.exprs.alloc(expr)
    });

    (
        PDecl::Fn(PFnDecl {
            vis_opt,
            keyword,
            name_opt,
            param_list_opt,
            arrow_opt,
            result_ty_opt,
            block_opt,
            fn_id_opt: None,
        }),
        ADecl::Fn(AFnLikeDecl {
            modifiers,
            name_opt: a_name_opt,
            params,
            result_ty_opt: a_ty_opt,
            body_opt,
        }),
        event.end(PElementKind::FnDecl, px),
    )
}

pub(crate) fn alloc_extern_fn_decl(
    modifiers: AfterDeclModifiers,
    extern_keyword: PToken,
    fn_keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    param_list_opt: Option<AfterParamList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<AfterTy>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);
    let (param_list_opt, params) = match param_list_opt {
        Some((param_list, params)) => (Some(param_list), params),
        None => (None, vec![]),
    };
    let (result_ty_opt, a_ty_opt) = decompose_opt(result_ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));

    (
        PDecl::ExternFn(PExternFnDecl {
            extern_keyword,
            fn_keyword,
            name_opt,
            param_list_opt,
            arrow_opt,
            result_ty_opt,
            semi_opt,
            extern_fn_id_opt: None,
        }),
        ADecl::ExternFn(AFnLikeDecl {
            modifiers,
            name_opt: a_name_opt,
            params,
            result_ty_opt: a_ty_opt,
            body_opt: None,
        }),
        event.end(PElementKind::ExternFnDecl, px),
    )
}

pub(crate) fn alloc_const_variant_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    equal_opt: Option<PToken>,
    init_opt: Option<AfterExpr>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterVariantDecl {
    let (name, a_name, _) = name;
    let (init_opt, a_init_opt) = decompose_opt(init_opt);
    let a_init_opt = a_init_opt.map(|expr| px.ast.exprs.alloc(expr));

    (
        PVariantDecl::Const(PConstVariantDecl {
            name,
            equal_opt,
            value_opt: init_opt,
            comma_opt,
            const_variant_id_opt: None,
        }),
        AVariantDecl::Const(AFieldLikeDecl {
            modifiers: ADeclModifiers::default(),
            name_opt: Some(a_name),
            ty_opt: None,
            value_opt: a_init_opt,
        }),
        event.end(PElementKind::ConstVariantDecl, px),
    )
}

pub(crate) fn alloc_field_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    ty_opt: Option<AfterTy>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterFieldDecl {
    let (name, a_name, _) = name;
    let (ty_opt, a_ty_opt) = decompose_opt(ty_opt);
    let a_ty_opt = a_ty_opt.map(|ty| px.ast.tys.alloc(ty));

    (
        PFieldDecl {
            name,
            colon_opt,
            ty_opt,
            comma_opt,
            field_id_opt: None,
        },
        AFieldLikeDecl {
            modifiers: ADeclModifiers::default(),
            name_opt: Some(a_name),
            ty_opt: a_ty_opt,
            value_opt: None,
        },
        event.end(PElementKind::FieldDecl, px),
    )
}

pub(crate) fn alloc_field_decls(fields: Vec<AfterFieldDecl>, _px: &mut Px) -> AfterFieldDecls {
    fields
        .into_iter()
        .map(|(field, field_decl, _)| (field, field_decl))
        .unzip()
}

pub(crate) fn alloc_record_variant_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    left_brace: PToken,
    fields: AfterFieldDecls,
    right_brace_opt: Option<PToken>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterVariantDecl {
    let (name, a_name, _) = name;
    let (fields, field_decls) = fields;

    (
        PVariantDecl::Record(PRecordVariantDecl {
            name,
            left_brace,
            fields,
            right_brace_opt,
            comma_opt,
        }),
        AVariantDecl::Record(ARecordVariantDecl {
            name: a_name,
            fields: field_decls,
        }),
        event.end(PElementKind::RecordVariantDecl, px),
    )
}

pub(crate) fn alloc_variants(variants: Vec<AfterVariantDecl>, _px: &mut Px) -> AfterVariantDecls {
    variants
        .into_iter()
        .map(|(variant, variant_decl, _)| (variant, variant_decl))
        .unzip()
}

pub(crate) fn alloc_enum_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    left_brace_opt: Option<PToken>,
    variants: AfterVariantDecls,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (_, vis_opt) = modifiers;
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);
    let (variants, variant_decls) = variants;

    (
        PDecl::Enum(PEnumDecl {
            vis_opt,
            keyword,
            name_opt,
            left_brace_opt,
            variants,
            right_brace_opt,
        }),
        ADecl::Enum(AEnumDecl {
            modifiers,
            name_opt: a_name_opt,
            variants: variant_decls,
        }),
        event.end(PElementKind::EnumDecl, px),
    )
}

pub(crate) fn alloc_struct_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    variant_opt: Option<AfterVariantDecl>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (variant_opt, a_variant_opt) = decompose_opt(variant_opt);

    (
        PDecl::Struct(PStructDecl {
            keyword,
            variant_opt,
            semi_opt,
        }),
        ADecl::Struct(AStructDecl {
            modifiers,
            variant_opt: a_variant_opt,
        }),
        event.end(PElementKind::StructDecl, px),
    )
}

pub(crate) fn alloc_use_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterQualifiableName>,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    let (event, modifiers) = alloc_modifiers(modifiers);
    let (name_opt, a_name_opt) = decompose_opt(name_opt);

    (
        PDecl::Use(PUseDecl {
            keyword,
            name_opt,
            semi_opt,
        }),
        ADecl::Use(AUseDecl {
            modifiers,
            name_opt: a_name_opt,
        }),
        event.end(PElementKind::UseDecl, px),
    )
}
