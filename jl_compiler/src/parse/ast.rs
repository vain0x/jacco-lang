//! 抽象構文木 (abstract syntax tree; AST)

use super::{PBinaryOp, PElement, PToken, PUnaryOp};
use crate::{
    cps::{KMut, KVis},
    utils::{VecArena, VecArenaId, VecArenaSlice},
};

// -----------------------------------------------
// 名前
// -----------------------------------------------

pub(crate) struct AName {
    pub(crate) element: PElement,
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) struct ATyTag;
pub(crate) type ATyId = VecArenaId<ATyTag>;
pub(crate) type ATyArena = VecArena<ATyTag, ATy>;

pub(crate) struct APtrTy {
    pub(crate) element: PElement,
    pub(crate) mut_opt: Option<KMut>,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) enum ATy {
    Name(AName),
    Never(PElement),
    Unit(PElement),
    Ptr(APtrTy),
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

pub(crate) struct APatTag;
pub(crate) type APatId = VecArenaId<APatTag>;
pub(crate) type APatArena = VecArena<APatTag, APat>;

pub(crate) struct AFieldPat {
    pub(crate) element: PElement,
    pub(crate) name: PToken,
    pub(crate) pat_opt: Option<APatId>,
}

pub(crate) struct ARecordPat {
    pub(crate) element: PElement,
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
    Unit(PElement),
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
    pub(crate) element: PElement,
    pub(crate) left: AExprId,
    pub(crate) field_opt: Option<PToken>,
}

/// `f()` or `a[]`
pub(crate) struct ACallLikeExpr {
    pub(crate) element: PElement,
    pub(crate) left: AExprId,
    pub(crate) args: AExprIds,
}

pub(crate) struct AAsExpr {
    pub(crate) element: PElement,
    pub(crate) left: AExprId,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) struct AUnaryOpExpr {
    pub(crate) element: PElement,
    pub(crate) op: PUnaryOp,
    pub(crate) mut_opt: Option<KMut>,
    pub(crate) arg_opt: Option<AExprId>,
}

pub(crate) struct ABinaryOpExpr {
    pub(crate) element: PElement,
    pub(crate) op: PBinaryOp,
    pub(crate) left: AExprId,
    pub(crate) right_opt: Option<AExprId>,
}

pub(crate) struct APipeExpr {
    pub(crate) element: PElement,
    pub(crate) left: AExprId,
    pub(crate) right_opt: Option<AExprId>,
}

pub(crate) struct AFieldExpr {
    pub(crate) element: PElement,
    pub(crate) field_name: PToken,
    pub(crate) value_opt: Option<AExprId>,
}

pub(crate) struct ARecordExpr {
    pub(crate) element: PElement,
    pub(crate) left: AName,
    pub(crate) fields: Vec<AFieldExpr>,
}

pub(crate) struct AJumpExpr {
    pub(crate) element: PElement,
    pub(crate) arg_opt: Option<AExprId>,
}

pub(crate) struct ABlockExpr {
    pub(crate) element: PElement,
    pub(crate) decls: ADeclIds,
}

pub(crate) struct AIfExpr {
    pub(crate) element: PElement,
    pub(crate) cond_opt: Option<AExprId>,
    pub(crate) body_opt: Option<AExprId>,
    pub(crate) alt_opt: Option<AExprId>,
}

pub(crate) struct AArm {
    pub(crate) element: PElement,
    pub(crate) pat_opt: Option<APatId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct AMatchExpr {
    pub(crate) element: PElement,
    pub(crate) arms: Vec<AArm>,
}

pub(crate) struct AWhileExpr {
    pub(crate) element: PElement,
    pub(crate) cond_opt: Option<AExprId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct ALoopExpr {
    pub(crate) element: PElement,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) enum AExpr {
    Number(PElement),
    Char(PElement),
    Str(PElement),
    True(PElement),
    False(PElement),
    Name(AName),
    Unit(PElement),
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
    Continue(AJumpExpr),
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

pub(crate) struct ADeclModifiers {
    pub(crate) vis_opt: Option<KVis>,
    // attrs: Vec<AAttr>,
}

/// let, const, static, const variant, field of record variant
pub(crate) struct AFieldLikeDecl {
    pub(crate) element: PElement,
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name: PToken,
    pub(crate) ty_opt: Option<ATyId>,
    pub(crate) value_opt: Option<AExprId>,
}

pub(crate) struct AExprDecl {
    pub(crate) element: PElement,
    pub(crate) expr: AExprId,
}

pub(crate) struct AParamDecl {
    pub(crate) element: PElement,
    pub(crate) name: PToken,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) struct AFnLikeDecl {
    pub(crate) element: PElement,
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<PToken>,
    pub(crate) params: Vec<AParamDecl>,
    pub(crate) result_ty_opt: Option<ATyId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct ARecordVariantDecl {
    pub(crate) element: PElement,
    pub(crate) left: AName,
    pub(crate) fields: Vec<AFieldLikeDecl>,
}

pub(crate) enum AVariantDecl {
    Const(AFieldLikeDecl),
    Record(ARecordVariantDecl),
}

pub(crate) struct AEnumDecl {
    pub(crate) element: PElement,
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<PToken>,
    pub(crate) variants: Vec<AVariantDecl>,
}

pub(crate) struct AStructDecl {
    pub(crate) element: PElement,
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) variant_opt: Option<AVariantDecl>,
}

pub(crate) struct AUseDecl {
    pub(crate) element: PElement,
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

pub(crate) struct ARoot {
    pub(crate) decls: ADeclIds,
}

pub(crate) struct ATree {
    root: ARoot,
    tys: ATyArena,
    pats: APatArena,
    exprs: AExprArena,
    decls: ADeclArena,
}
