//! 抽象構文木 (abstract syntax tree; AST)

#![allow(unused)]

use super::{PBinaryOp, PToken, PUnaryOp};
use crate::{
    cps::{KMut, KVis},
    utils::{VecArena, VecArenaId, VecArenaSlice},
};
use std::fmt::{self, Debug, Formatter};

// -----------------------------------------------
// 名前
// -----------------------------------------------

pub(crate) struct AName;

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) struct ATyTag;
pub(crate) type ATyId = VecArenaId<ATyTag>;
pub(crate) type ATyArena = VecArena<ATyTag, ATy>;

pub(crate) struct APtrTy {
    pub(crate) mut_opt: Option<KMut>,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) enum ATy {
    Name(AName),
    Never,
    Unit,
    Ptr(APtrTy),
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

pub(crate) struct APatTag;
pub(crate) type APatId = VecArenaId<APatTag>;
pub(crate) type APatArena = VecArena<APatTag, APat>;

pub(crate) struct AFieldPat {
    pub(crate) name: PToken,
    pub(crate) pat_opt: Option<APatId>,
}

pub(crate) struct ARecordPat {
    pub(crate) left: AName,
    pub(crate) fields: Vec<AFieldPat>,
}

pub(crate) enum APat {
    Number(PToken),
    Char(PToken),
    Str(PToken),
    True(PToken),
    False(PToken),
    Discard(PToken),
    Name(AName),
    Unit,
    Record(ARecordPat),
}

// -----------------------------------------------
// 式
// -----------------------------------------------

pub(crate) struct AExprTag;
pub(crate) type AExprId = VecArenaId<AExprTag>;
pub(crate) type AExprIds = VecArenaSlice<AExprTag>;
pub(crate) type AExprArena = VecArena<AExprTag, AExpr>;

pub(crate) struct ADotFieldExpr {
    pub(crate) left: AExprId,
    pub(crate) field_opt: Option<PToken>,
}

/// `f()` or `a[]`
pub(crate) struct ACallLikeExpr {
    pub(crate) left: AExprId,
    pub(crate) args: AExprIds,
}

pub(crate) struct AAsExpr {
    pub(crate) left: AExprId,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) struct AUnaryOpExpr {
    pub(crate) op: PUnaryOp,
    pub(crate) mut_opt: Option<KMut>,
    pub(crate) arg_opt: Option<AExprId>,
}

pub(crate) struct ABinaryOpExpr {
    pub(crate) op: PBinaryOp,
    pub(crate) left: AExprId,
    pub(crate) right_opt: Option<AExprId>,
}

pub(crate) struct APipeExpr {
    pub(crate) left: AExprId,
    pub(crate) right_opt: Option<AExprId>,
}

pub(crate) struct AFieldExpr {
    pub(crate) field_name: PToken,
    pub(crate) value_opt: Option<AExprId>,
}

pub(crate) struct ARecordExpr {
    pub(crate) left: AName,
    pub(crate) fields: Vec<AFieldExpr>,
}

pub(crate) struct AJumpExpr {
    pub(crate) arg_opt: Option<AExprId>,
}

pub(crate) struct ABlockExpr {
    pub(crate) decls: ADeclIds,
}

pub(crate) struct AIfExpr {
    pub(crate) cond_opt: Option<AExprId>,
    pub(crate) body_opt: Option<AExprId>,
    pub(crate) alt_opt: Option<AExprId>,
}

pub(crate) struct AArm {
    pub(crate) pat: APatId,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct AMatchExpr {
    pub(crate) cond_opt: Option<AExprId>,
    pub(crate) arms: Vec<AArm>,
}

pub(crate) struct AWhileExpr {
    pub(crate) cond_opt: Option<AExprId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct ALoopExpr {
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) enum AExpr {
    Number(PToken),
    Char(PToken),
    Str(PToken),
    True,
    False,
    Name(AName),
    Unit,
    Record(ARecordExpr),
    DotField(ADotFieldExpr),
    Call(ACallLikeExpr),
    Index(ACallLikeExpr),
    As(AAsExpr),
    UnaryOp(AUnaryOpExpr),
    BinaryOp(ABinaryOpExpr),
    Pipe(APipeExpr),
    Block(ABlockExpr),
    Break(AJumpExpr),
    Continue,
    Return(AJumpExpr),
    If(AIfExpr),
    Match(AMatchExpr),
    While(AWhileExpr),
    Loop(ALoopExpr),
}

/// -----------------------------------------------
// 宣言
// -----------------------------------------------

pub(crate) struct ADeclTag;
pub(crate) type ADeclId = VecArenaId<ADeclTag>;
pub(crate) type ADeclIds = VecArenaSlice<ADeclTag>;
pub(crate) type ADeclArena = VecArena<ADeclTag, ADecl>;

#[derive(Default)]
pub(crate) struct ADeclModifiers {
    pub(crate) vis_opt: Option<KVis>,
    // attrs: Vec<AAttr>,
}

/// let, const, static, const variant, field of record variant
pub(crate) struct AFieldLikeDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<PToken>,
    pub(crate) ty_opt: Option<ATyId>,
    pub(crate) value_opt: Option<AExprId>,
}

pub(crate) struct AExprDecl {
    pub(crate) expr: AExprId,
}

pub(crate) struct AParamDecl {
    pub(crate) name: PToken,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) struct AFnLikeDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<PToken>,
    pub(crate) params: Vec<AParamDecl>,
    pub(crate) result_ty_opt: Option<ATyId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct ARecordVariantDecl {
    pub(crate) left: PToken,
    pub(crate) fields: Vec<AFieldLikeDecl>,
}

pub(crate) enum AVariantDecl {
    Const(AFieldLikeDecl),
    Record(ARecordVariantDecl),
}

pub(crate) struct AEnumDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<PToken>,
    pub(crate) variants: Vec<AVariantDecl>,
}

pub(crate) struct AStructDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) variant_opt: Option<AVariantDecl>,
}

pub(crate) struct AUseDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<AName>,
}

pub(crate) enum ADecl {
    Expr(AExprId),
    Let(AFieldLikeDecl),
    Const(AFieldLikeDecl),
    Static(AFieldLikeDecl),
    Fn(AFnLikeDecl),
    ExternFn(AFnLikeDecl),
    Enum(AEnumDecl),
    Struct(AStructDecl),
    Use(AUseDecl),
}

// -----------------------------------------------
// ルート
// -----------------------------------------------

#[derive(Default)]
pub(crate) struct ARoot {
    pub(crate) decls: ADeclIds,
}

#[derive(Default)]
pub(crate) struct ATree {
    pub(super) root: ARoot,
    pub(super) tys: ATyArena,
    pub(super) pats: APatArena,
    pub(super) exprs: AExprArena,
    pub(super) decls: ADeclArena,
}

impl Clone for ATree {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl Debug for ATree {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

fn traverse_ast(p_root: &super::PRoot) {
    let ast = &p_root.ast;
    for decl in ast.root.decls.of(&ast.decls) {
        match decl {
            ADecl::Expr(expr) => {
                let e = expr.of(&ast.exprs);

                eprintln!("expr");
            }
            ADecl::Let(AFieldLikeDecl { .. }) => {
                eprintln!("let");
            }
            _ => {}
        }
    }
}
