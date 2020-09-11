#![allow(unused)]

use super::*;
use crate::{front::name_resolution, parse::syntax_error::*};

pub(crate) type AfterQualifiableName = (AName, NameEnd);
pub(crate) type AfterUnderscore = (AName, NameEnd);
pub(crate) type AfterUnqualifiableName = (AName, NameEnd);
pub(crate) type AfterTyParam = (ATyParamDecl, Option<PToken>, ParseEnd);
pub(crate) type AfterTyParamList = Vec<ATyParamDecl>;
pub(crate) type AfterTyArg = (AfterTy, Option<PToken>, ParseEnd);
pub(crate) type AfterTyArgList = Vec<AfterTy>;
pub(crate) type AfterParamTy = (AfterTy, Option<PToken>, ParseEnd);
pub(crate) type AfterParamTyList = Vec<AfterTy>;
pub(crate) type AfterParam = (AParamDecl, Option<PToken>, ParseEnd);
pub(crate) type AfterParamList = Vec<AfterParam>;
pub(crate) type AfterArg = (AExpr, ExprEnd);
pub(crate) type AfterArgList = Vec<(AExpr, ExprEnd)>;
pub(crate) type AfterTy = (ATy, TyEnd);
pub(crate) type AfterPat = (APat, PatEnd);
pub(crate) type AfterLabeledArg = (ALabeledArg, ParseEnd);
pub(crate) type AfterArm = (AArm, ParseEnd);
pub(crate) type AfterExpr = (AExpr, ExprEnd);
pub(crate) type AfterBlock = (Vec<(ADecl, DeclEnd)>, ExprEnd);
pub(crate) type AfterDeclModifiers = (DeclStart, Option<PVis>);
pub(crate) type AfterVariantDecl = (AVariantDecl, ParseEnd);
pub(crate) type AfterVariantDecls = Vec<AVariantDecl>;
pub(crate) type AfterFieldDecl = (AFieldLikeDecl, ParseEnd);
pub(crate) type AfterFieldDecls = Vec<AFieldLikeDecl>;
pub(crate) type AfterDecl = (ADecl, DeclEnd);
pub(crate) type AfterSemi = Vec<(ADecl, DeclEnd)>;
pub(crate) type AfterRoot = Vec<AfterDecl>;

impl Px {
    fn alloc_name(&mut self, (name, event): (AName, NameEnd)) -> ANameId {
        self.ast.name_events.alloc(event);
        self.ast.names.alloc(name)
    }

    fn alloc_ty(&mut self, (ty, event): (ATy, TyEnd)) -> ATyId {
        self.ast.ty_events.alloc(event);
        self.ast.tys.alloc(ty)
    }

    fn alloc_tys(&mut self, tys: Vec<(ATy, TyEnd)>) -> ATyIds {
        let (tys, events): (Vec<_>, Vec<_>) = tys.into_iter().unzip();
        self.ast.ty_events.alloc_slice(events);
        self.ast.tys.alloc_slice(tys)
    }

    fn alloc_pat(&mut self, (pat, event): (APat, PatEnd)) -> APatId {
        self.ast.pat_events.alloc(event);
        self.ast.pats.alloc(pat)
    }

    fn alloc_expr(&mut self, (expr, event): (AExpr, ExprEnd)) -> AExprId {
        self.ast.expr_events.alloc(event);
        self.ast.exprs.alloc(expr)
    }

    fn alloc_exprs(&mut self, exprs: Vec<(AExpr, ExprEnd)>) -> AExprIds {
        let (exprs, events): (Vec<_>, Vec<_>) = exprs.into_iter().unzip();
        self.ast.expr_events.alloc_slice(events);
        self.ast.exprs.alloc_slice(exprs)
    }

    pub(crate) fn alloc_decls(&mut self, decls: Vec<(ADecl, DeclEnd)>) -> ADeclIds {
        let (decls, events): (Vec<_>, Vec<_>) = decls.into_iter().unzip();
        self.ast.decl_events.alloc_slice(events);
        self.ast.decls.alloc_slice(decls)
    }
}

// -----------------------------------------------
// 名前
// -----------------------------------------------

pub(crate) fn alloc_name(
    event: NameStart,
    quals: Vec<PToken>,
    token: PToken,
    px: &mut Px,
) -> AfterQualifiableName {
    // text を参照する箇所に tokens を渡すのがめんどくさいので、ここで文字列にしておく。文字列は intern される予定なのでコピーのコストは考えなくてよい。
    let text = token.text(px.tokens()).to_string();

    (
        AName { quals, token, text },
        event.end(PElementKind::Name, px),
    )
}

pub(crate) fn alloc_name_from_underscore(
    event: NameStart,
    token: PToken,
    px: &mut Px,
) -> AfterQualifiableName {
    (
        AName {
            quals: vec![],
            token,
            text: "_".to_string(),
        },
        event.end(PElementKind::Name, px),
    )
}

// -----------------------------------------------
// パラメータ・引数
// -----------------------------------------------

pub(crate) fn alloc_ty_param(
    event: ParseStart,
    name: AfterUnqualifiableName,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterTyParam {
    // FIXME: 構文エラーを報告する

    let name = px.alloc_name(name);

    (
        ATyParamDecl { name },
        comma_opt,
        event.end(PElementKind::TyParam, px),
    )
}

pub(crate) fn alloc_ty_param_list(
    left_bracket: PToken,
    ty_params: Vec<AfterTyParam>,
    right_bracket_opt: Option<PToken>,
    px: &mut Px,
) -> AfterTyParamList {
    // FIXME: 構文エラーを報告する

    ty_params
        .into_iter()
        .map(|(ty_param, _, _)| ty_param)
        .collect()
}

pub(crate) fn alloc_ty_arg(
    event: ParseStart,
    ty: AfterTy,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterParamTy {
    (ty, comma_opt, event.end(PElementKind::TyArg, px))
}

pub(crate) fn alloc_ty_arg_list(
    left_bracket: PToken,
    ty_args: Vec<AfterTyArg>,
    right_bracket_opt: Option<PToken>,
    px: &mut Px,
) -> AfterTyArgList {
    // FIXME: 構文エラーを報告する

    ty_args.into_iter().map(|(ty, _, _)| ty).collect()
}

pub(crate) fn alloc_param_ty(
    event: ParseStart,
    ty: AfterTy,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterParamTy {
    (ty, comma_opt, event.end(PElementKind::ParamTy, px))
}

pub(crate) fn alloc_param_ty_list(
    left_paren: PToken,
    param_tys: Vec<AfterParamTy>,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterParamTyList {
    // FIXME: 構文エラーを報告する

    param_tys.into_iter().map(|(ty, _, _)| ty).collect()
}

pub(crate) fn alloc_param(
    event: ParseStart,
    name: AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    ty_opt: Option<AfterTy>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterParam {
    validate_param(&name, colon_opt, ty_opt.as_ref(), px);

    let name = px.alloc_name(name);
    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));

    name_resolution::v3::on_param_decl(name, &px.ast, &mut px.name_resolver);

    (
        AParamDecl {
            name,
            ty_opt: a_ty_opt,
        },
        comma_opt,
        event.end(PElementKind::ParamDecl, px),
    )
}

pub(crate) fn alloc_param_list(
    left_paren: PToken,
    params: Vec<AfterParam>,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterParamList {
    validate_param_list(left_paren, &params, right_paren_opt, px);

    params
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) fn alloc_name_ty(event: TyStart, name: AfterQualifiableName, px: &mut Px) -> AfterTy {
    let name = px.alloc_name(name);

    (ATy::Name(name), event.end(PElementKind::NameTy, px))
}

pub(crate) fn alloc_app_ty(
    event: TyStart,
    name: AfterQualifiableName,
    ty_args: AfterTyArgList,
    px: &mut Px,
) -> AfterTy {
    let name = px.alloc_name(name);
    let ty_args = px.alloc_tys(ty_args);

    (ATy::App(name, ty_args), event.end(PElementKind::AppTy, px))
}

pub(crate) fn alloc_infer_ty(event: TyStart, token: PToken, px: &mut Px) -> AfterTy {
    (ATy::InferTy, event.end(PElementKind::InferTy, px))
}

pub(crate) fn alloc_unit_ty(event: TyStart, _token: PToken, px: &mut Px) -> AfterTy {
    (ATy::Unit, event.end(PElementKind::UnitTy, px))
}

pub(crate) fn alloc_unit_ty_from_parens(
    event: TyStart,
    left_paren: PToken,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterTy {
    (ATy::Unit, event.end(PElementKind::UnitTy, px))
}

pub(crate) fn alloc_never_ty(event: TyStart, bang: PToken, px: &mut Px) -> AfterTy {
    (ATy::Never, event.end(PElementKind::NeverTy, px))
}

pub(crate) fn alloc_ptr_ty(
    event: TyStart,
    star: PToken,
    mut_opt: Option<PMut>,
    ty_opt: Option<AfterTy>,
    px: &mut Px,
) -> AfterTy {
    validate_ptr_ty(star, mut_opt, ty_opt.as_ref(), px);

    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));

    (
        ATy::Ptr(APtrTy {
            mut_opt: mut_opt.map(|p| p.0),
            ty_opt: a_ty_opt,
        }),
        event.end(PElementKind::PtrTy, px),
    )
}

pub(crate) fn alloc_fn_ty(
    event: TyStart,
    keyword: PToken,
    param_ty_list_opt: Option<AfterParamTyList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<AfterTy>,
    px: &mut Px,
) -> AfterTy {
    // FIXME: 構文エラーを報告する

    let param_tys = param_ty_list_opt
        .into_iter()
        .flatten()
        .map(|ty| px.alloc_ty(ty))
        .collect();
    let result_ty_opt = result_ty_opt.map(|ty| px.alloc_ty(ty));

    (
        ATy::Fn(AFnTy {
            param_tys,
            result_ty_opt,
        }),
        event.end(PElementKind::FnTy, px),
    )
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

pub(crate) fn alloc_char_pat(event: PatStart, token: PToken, px: &mut Px) -> AfterPat {
    (APat::Char(token), event.end(PElementKind::CharPat, px))
}

pub(crate) fn alloc_wildcard_pat(event: PatStart, token: PToken, px: &mut Px) -> AfterPat {
    (
        APat::Wildcard(token),
        event.end(PElementKind::WildcardPat, px),
    )
}

pub(crate) fn alloc_name_pat(event: PatStart, name: AfterQualifiableName, px: &mut Px) -> AfterPat {
    let name = px.alloc_name(name);

    (APat::Name(name), event.end(PElementKind::NamePat, px))
}

pub(crate) fn alloc_record_pat(
    event: PatStart,
    name: AfterQualifiableName,
    left_brace: PToken,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterPat {
    validate_record_pat(&name, left_brace, right_brace_opt, px);

    let name = px.alloc_name(name);

    (
        APat::Record(ARecordPat {
            left: name,
            fields: vec![],
        }),
        event.end(PElementKind::RecordPat, px),
    )
}

// -----------------------------------------------
// 式
// -----------------------------------------------

pub(crate) fn alloc_paren_expr(
    event: ExprStart,
    left_paren: PToken,
    body_opt: Option<AfterExpr>,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    validate_paren_expr(left_paren, right_paren_opt, px);

    match body_opt {
        Some(a_expr) => {
            event.end(PElementKind::ParenExpr, px);
            a_expr
        }
        None => ((AExpr::Unit, event.end(PElementKind::UnitExpr, px))),
    }
}

pub(crate) fn alloc_number(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (
        AExpr::Number(token),
        event.end(PElementKind::NumberExpr, px),
    )
}

pub(crate) fn alloc_char(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (AExpr::Char(token), event.end(PElementKind::CharExpr, px))
}

pub(crate) fn alloc_str(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (AExpr::Str(token), event.end(PElementKind::StrExpr, px))
}

pub(crate) fn alloc_unit_expr(event: ExprStart, _token: PToken, px: &mut Px) -> AfterExpr {
    (AExpr::Unit, event.end(PElementKind::UnitExpr, px))
}

pub(crate) fn alloc_unit_expr_from_parens(
    event: ExprStart,
    _left_paren: PToken,
    _right_paren: PToken,
    px: &mut Px,
) -> AfterExpr {
    (AExpr::Unit, event.end(PElementKind::UnitExpr, px))
}

pub(crate) fn alloc_true(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (AExpr::True, event.end(PElementKind::TrueExpr, px))
}

pub(crate) fn alloc_false(event: ExprStart, token: PToken, px: &mut Px) -> AfterExpr {
    (AExpr::False, event.end(PElementKind::FalseExpr, px))
}

pub(crate) fn alloc_name_expr(
    event: ExprStart,
    name: AfterQualifiableName,
    px: &mut Px,
) -> AfterExpr {
    let name = px.alloc_name(name);

    name_resolution::v3::on_name_expr(name, &px.ast, &mut px.name_resolver);

    (AExpr::Name(name), event.end(PElementKind::NameExpr, px))
}

pub(crate) fn alloc_record_expr(
    event: ExprStart,
    name: AfterQualifiableName,
    left_brace: PToken,
    fields: Vec<AfterLabeledArg>,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    validate_record_expr(left_brace, right_brace_opt, px);

    let name = px.alloc_name(name);
    let a_fields = fields
        .into_iter()
        .map(|(field_expr, _)| field_expr)
        .collect();

    (
        AExpr::Record(ARecordExpr {
            left: name,
            fields: a_fields,
        }),
        event.end(PElementKind::RecordExpr, px),
    )
}

pub(crate) fn alloc_field_expr(
    event: ExprStart,
    left: AfterExpr,
    dot: PToken,
    name_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    validate_field_expr(&left, dot, name_opt, px);

    let a_left = px.alloc_expr(left);

    (
        AExpr::Field(AFieldExpr {
            left: a_left,
            field_opt: name_opt,
        }),
        event.end(PElementKind::FieldExpr, px),
    )
}

pub(crate) fn alloc_call_expr(
    event: ExprStart,
    left: AfterExpr,
    arg_list: AfterArgList,
    px: &mut Px,
) -> AfterExpr {
    let a_left = px.alloc_expr(left);
    let args = px.alloc_exprs(arg_list);

    (
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
    let a_left = px.alloc_expr(left);
    let args = px.alloc_exprs(arg_list);

    (
        AExpr::Index(ACallLikeExpr { left: a_left, args }),
        event.end(PElementKind::IndexExpr, px),
    )
}

pub(crate) fn alloc_cast_expr(
    event: ExprStart,
    left: AfterExpr,
    keyword: PToken,
    ty_opt: Option<AfterTy>,
    px: &mut Px,
) -> AfterExpr {
    validate_cast_expr(&left, keyword, ty_opt.as_ref(), px);

    let a_left = px.alloc_expr(left);
    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));

    (
        AExpr::Cast(ACastExpr {
            left: a_left,
            ty_opt: a_ty_opt,
        }),
        event.end(PElementKind::CastExpr, px),
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
    validate_prefix_expr(token, mut_opt, arg_opt.as_ref(), px);

    let a_arg_opt = arg_opt.map(|expr| px.alloc_expr(expr));

    (
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
    validate_binary_op_expr(token, right_opt.as_ref(), px);

    let a_left = px.alloc_expr(left);
    let a_right_opt = right_opt.map(|expr| px.alloc_expr(expr));

    (
        AExpr::BinaryOp(ABinaryOpExpr {
            op: binary_op,
            left: a_left,
            right_opt: a_right_opt,
        }),
        event.end(PElementKind::BinaryOpExpr, px),
    )
}

fn do_alloc_block_expr(event: ExprEnd, decls: Vec<(ADecl, DeclEnd)>, px: &mut Px) -> AExprId {
    let decls = px.alloc_decls(decls);
    px.alloc_expr((AExpr::Block(ABlockExpr { decls }), event))
}

pub(crate) fn before_block(px: &mut Px) {
    name_resolution::v3::enter_block(&mut px.name_resolver);
}

pub(crate) fn alloc_block(
    event: ExprStart,
    left_brace: PToken,
    semi: AfterSemi,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterBlock {
    let a_decls = semi;

    name_resolution::v3::leave_block(&mut px.name_resolver);

    (a_decls, event.end(PElementKind::BlockExpr, px))
}

pub(crate) fn alloc_block_expr(
    event: ExprStart,
    left_brace: PToken,
    semi: AfterSemi,
    right_brace_opt: Option<PToken>,
    px: &mut Px,
) -> AfterExpr {
    validate_block_expr(left_brace, right_brace_opt, px);

    let a_decls = px.alloc_decls(semi);

    (
        AExpr::Block(ABlockExpr { decls: a_decls }),
        event.end(PElementKind::BlockExpr, px),
    )
}

pub(crate) fn alloc_break_expr(
    event: ExprStart,
    keyword: PToken,
    arg_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let a_arg_opt = arg_opt.map(|expr| px.alloc_expr(expr));

    (
        AExpr::Break(AJumpExpr { arg_opt: a_arg_opt }),
        event.end(PElementKind::BreakExpr, px),
    )
}

pub(crate) fn alloc_continue_expr(event: ExprStart, keyword: PToken, px: &mut Px) -> AfterExpr {
    (AExpr::Continue, event.end(PElementKind::ContinueExpr, px))
}

pub(crate) fn alloc_return_expr(
    event: ExprStart,
    keyword: PToken,
    arg_opt: Option<AfterExpr>,
    px: &mut Px,
) -> AfterExpr {
    let a_arg_opt = arg_opt.map(|expr| px.alloc_expr(expr));

    (
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
    validate_if_expr(
        keyword,
        cond_opt.as_ref(),
        body_opt.as_ref(),
        else_opt,
        alt_opt.as_ref(),
        px,
    );

    let a_cond_opt = cond_opt.map(|expr| px.alloc_expr(expr));
    let a_body_opt = body_opt.map(|(decls, body_event)| do_alloc_block_expr(body_event, decls, px));
    let a_alt_opt = alt_opt.map(|expr| px.alloc_expr(expr));

    (
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
    validate_arm(&pat, arrow_opt, body_opt.as_ref(), comma_opt, px);

    let a_pat = px.alloc_pat(pat);
    let a_body_opt = body_opt.map(|expr| px.alloc_expr(expr));

    (
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
    validate_match_expr(
        keyword,
        cond_opt.as_ref(),
        left_brace_opt,
        &arms,
        right_brace_opt,
        px,
    );

    let a_cond_opt = cond_opt.map(|expr| px.alloc_expr(expr));
    let a_arms = arms.into_iter().map(|(a_arm, _)| a_arm).collect();

    (
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
    validate_while_expr(keyword, cond_opt.as_ref(), body_opt.as_ref(), px);

    let a_cond_opt = cond_opt.map(|expr| px.alloc_expr(expr));
    let a_body_opt = body_opt.map(|(decls, body_event)| do_alloc_block_expr(body_event, decls, px));

    (
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
    validate_loop_expr(keyword, body_opt.as_ref(), px);

    let a_body_opt = body_opt.map(|(decls, body_event)| do_alloc_block_expr(body_event, decls, px));

    (
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
    event.end(PElementKind::Arg, px);
    expr
}

pub(crate) fn alloc_labeled_arg(
    event: ParseStart,
    name: AfterUnqualifiableName,
    colon_opt: Option<PToken>,
    value_opt: Option<AfterExpr>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterLabeledArg {
    // FIXME: 構文エラーを報告する

    let name = px.alloc_name(name);
    let a_value_opt = value_opt.map(|expr| px.alloc_expr(expr));

    (
        ALabeledArg {
            field_name: name,
            value_opt: a_value_opt,
        },
        event.end(PElementKind::Arg, px),
    )
}

pub(crate) fn alloc_arg_list(
    left_paren: PToken,
    args: Vec<AfterArg>,
    right_paren_opt: Option<PToken>,
    px: &mut Px,
) -> AfterArgList {
    validate_arg_list(left_paren, &args, right_paren_opt, px);

    args
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

pub(crate) fn alloc_attr_decl(event: DeclStart, hash_bang: PToken, px: &mut Px) -> AfterDecl {
    (ADecl::Attr, event.end(PElementKind::AttrDecl, px))
}

pub(crate) fn alloc_expr_decl(
    event: DeclStart,
    expr: AfterExpr,
    semi_opt: Option<PToken>,
    px: &mut Px,
) -> AfterDecl {
    // FIXME: セミコロンの抜けを報告する

    let a_expr = px.alloc_expr(expr);

    (ADecl::Expr(a_expr), event.end(PElementKind::ExprDecl, px))
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
    validate_let_decl(
        &modifiers.0,
        &modifiers,
        keyword,
        name_opt.as_ref(),
        colon_opt,
        ty_opt.as_ref(),
        equal_opt,
        init_opt.as_ref(),
        semi_opt,
        px,
    );

    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));
    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));
    let a_init_opt = init_opt.map(|expr| px.alloc_expr(expr));

    name_resolution::v3::leave_let_decl(name_opt, &px.ast, &mut px.name_resolver);

    (
        ADecl::Let(AFieldLikeDecl {
            modifiers,
            name_opt,
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
    validate_const_decl(
        &modifiers.0,
        &modifiers,
        keyword,
        name_opt.as_ref(),
        colon_opt,
        ty_opt.as_ref(),
        equal_opt,
        init_opt.as_ref(),
        semi_opt,
        px,
    );

    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));
    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));
    let a_init_opt = init_opt.map(|expr| px.alloc_expr(expr));

    name_resolution::v3::leave_const_decl(name_opt, &px.ast, &mut px.name_resolver);

    (
        ADecl::Const(AFieldLikeDecl {
            modifiers,
            name_opt,
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
    validate_static_decl(
        &modifiers.0,
        &modifiers,
        keyword,
        name_opt.as_ref(),
        colon_opt,
        ty_opt.as_ref(),
        equal_opt,
        init_opt.as_ref(),
        semi_opt,
        px,
    );

    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));
    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));
    let a_init_opt = init_opt.map(|expr| px.alloc_expr(expr));

    name_resolution::v3::leave_static_decl(name_opt, &px.ast, &mut px.name_resolver);

    (
        ADecl::Static(AFieldLikeDecl {
            modifiers,
            name_opt,
            ty_opt: a_ty_opt,
            value_opt: a_init_opt,
        }),
        event.end(PElementKind::StaticDecl, px),
    )
}

pub(crate) fn before_fn_decl(px: &mut Px) {
    name_resolution::v3::enter_fn_decl(&mut px.name_resolver);
}

pub(crate) fn alloc_fn_decl(
    modifiers: AfterDeclModifiers,
    keyword: PToken,
    name_opt: Option<AfterUnqualifiableName>,
    ty_param_list_opt: Option<AfterTyParamList>,
    param_list_opt: Option<AfterParamList>,
    arrow_opt: Option<PToken>,
    result_ty_opt: Option<AfterTy>,
    block_opt: Option<AfterBlock>,
    px: &mut Px,
) -> AfterDecl {
    validate_fn_decl(
        &modifiers.0,
        &modifiers,
        keyword,
        name_opt.as_ref(),
        param_list_opt.as_ref(),
        arrow_opt,
        result_ty_opt.as_ref(),
        block_opt.as_ref(),
        px,
    );

    let (_, vis_opt) = modifiers;
    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));
    let ty_params = ty_param_list_opt.unwrap_or_default();
    let params = param_list_opt
        .into_iter()
        .flatten()
        .map(|(param, _, _)| param)
        .collect();
    let a_ty_opt = result_ty_opt.map(|ty| px.alloc_ty(ty));
    let body_opt = block_opt.map(|(decls, body_event)| do_alloc_block_expr(body_event, decls, px));

    name_resolution::v3::leave_fn_decl(name_opt, &px.ast, &mut px.name_resolver);

    (
        ADecl::Fn(AFnLikeDecl {
            modifiers,
            name_opt,
            ty_params,
            params,
            result_ty_opt: a_ty_opt,
            body_opt,
        }),
        event.end(PElementKind::FnDecl, px),
    )
}

pub(crate) fn before_extern_fn_decl(px: &mut Px) {
    name_resolution::v3::enter_extern_fn_decl(&mut px.name_resolver);
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
    validate_extern_fn_decl(
        &modifiers.0,
        &modifiers,
        extern_keyword,
        fn_keyword,
        name_opt.as_ref(),
        param_list_opt.as_ref(),
        arrow_opt,
        result_ty_opt.as_ref(),
        semi_opt,
        px,
    );

    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));
    let params = param_list_opt
        .into_iter()
        .flatten()
        .map(|(param, _, _)| param)
        .collect();
    let a_ty_opt = result_ty_opt.map(|ty| px.alloc_ty(ty));

    name_resolution::v3::leave_extern_fn_decl(name_opt, &px.ast, &mut px.name_resolver);

    (
        ADecl::ExternFn(AFnLikeDecl {
            modifiers,
            name_opt,
            // FIXME: 実装
            ty_params: vec![],
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
    validate_const_variant_decl(&name, equal_opt, init_opt.as_ref(), px);

    let name = px.alloc_name(name);
    let a_init_opt = init_opt.map(|expr| px.alloc_expr(expr));

    (
        AVariantDecl::Const(AFieldLikeDecl {
            modifiers: ADeclModifiers::default(),
            name_opt: Some(name),
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
    validate_field_decl(&name, colon_opt, ty_opt.as_ref(), px);

    let name = px.alloc_name(name);
    let a_ty_opt = ty_opt.map(|ty| px.alloc_ty(ty));

    (
        AFieldLikeDecl {
            modifiers: ADeclModifiers::default(),
            name_opt: Some(name),
            ty_opt: a_ty_opt,
            value_opt: None,
        },
        event.end(PElementKind::FieldDecl, px),
    )
}

pub(crate) fn alloc_field_decls(fields: Vec<AfterFieldDecl>, _px: &mut Px) -> AfterFieldDecls {
    fields.into_iter().map(|(field, _)| field).collect()
}

pub(crate) fn alloc_record_variant_decl(
    event: ParseStart,
    name: AfterUnqualifiableName,
    ty_param_list_opt: Option<AfterTyParamList>,
    left_brace: PToken,
    fields: AfterFieldDecls,
    right_brace_opt: Option<PToken>,
    comma_opt: Option<PToken>,
    px: &mut Px,
) -> AfterVariantDecl {
    validate_record_variant_decl(&name, left_brace, &fields, right_brace_opt, px);

    let name = px.alloc_name(name);
    let ty_params = ty_param_list_opt.unwrap_or_default();

    (
        AVariantDecl::Record(ARecordVariantDecl {
            name,
            ty_params,
            fields,
        }),
        event.end(PElementKind::RecordVariantDecl, px),
    )
}

pub(crate) fn alloc_variants(variants: Vec<AfterVariantDecl>, _px: &mut Px) -> AfterVariantDecls {
    variants
        .into_iter()
        .map(|(variant_decl, _)| variant_decl)
        .collect()
}

pub(crate) fn before_enum_decl(px: &mut Px) {
    name_resolution::v3::enter_enum_decl(&mut px.name_resolver);
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
    validate_enum_decl(
        &modifiers,
        keyword,
        name_opt.as_ref(),
        left_brace_opt,
        &variants,
        right_brace_opt,
        px,
    );

    let (_, vis_opt) = modifiers;
    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));

    name_resolution::v3::leave_enum_decl(name_opt, &px.ast, &mut px.name_resolver);

    (
        ADecl::Enum(AEnumDecl {
            modifiers,
            name_opt,
            variants,
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
    validate_struct_decl(&modifiers, keyword, variant_opt.as_ref(), semi_opt, px);

    let (event, modifiers) = alloc_modifiers(modifiers);
    let a_variant_opt = variant_opt.map(|(variant_decl, _)| variant_decl);

    (
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
    validate_use_decl(
        &modifiers.0,
        &modifiers,
        keyword,
        name_opt.as_ref(),
        semi_opt,
        px,
    );

    let (event, modifiers) = alloc_modifiers(modifiers);
    let name_opt = name_opt.map(|name| px.alloc_name(name));

    (
        ADecl::Use(AUseDecl {
            modifiers,
            name_opt,
        }),
        event.end(PElementKind::UseDecl, px),
    )
}
