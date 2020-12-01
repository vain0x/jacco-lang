//! 抽象構文木 (abstract syntax tree; AST)

#![allow(unused)]

use super::*;
use crate::{
    cps::{KMut, KVis},
    token::TokenKind,
    utils::{VecArena, VecArenaId, VecArenaSlice},
};
use std::fmt::{self, Debug, Formatter};

// -----------------------------------------------
// 名前
// -----------------------------------------------

// 識別子、あるいはパス

pub(crate) struct ANameTag;

pub(crate) type ANameId = VecArenaId<ANameTag>;

pub(crate) type ANameArena = VecArena<ANameTag, AName>;

pub(crate) struct AName {
    pub(crate) quals: Vec<PToken>,
    pub(crate) token: PToken,

    /// パスの先頭の識別子 (非修飾なら text と一致)
    // 先頭がないケース (`::name` など) はパーサ側で対処している。
    pub(crate) head: String,

    /// パスの末尾の識別子 (非修飾なら text と一致)
    // 末尾がないケース (`name::` など) はパーサ側で対処している。
    pub(crate) text: String,
}

impl AName {
    pub(crate) fn head(&self) -> &str {
        &self.head
    }

    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    pub(crate) fn is_qualified(&self) -> bool {
        !self.quals.is_empty()
    }
}

/// 型パラメータ
pub(crate) struct ATyParamDecl {
    pub(crate) name: ANameId,
}

// パラメータ型
pub(crate) struct AParamTyDecl {
    pub(crate) ty: ATy,
}

// -----------------------------------------------
// 型
// -----------------------------------------------

pub(crate) struct ATyTag;
pub(crate) type ATyId = VecArenaId<ATyTag>;
pub(crate) type ATyIds = VecArenaSlice<ATyTag>;
pub(crate) type ATyArena = VecArena<ATyTag, ATy>;

pub(crate) struct APtrTy {
    pub(crate) mut_opt: Option<KMut>,
    pub(crate) ty_opt: Option<ATyId>,
}

pub(crate) struct AFnTy {
    pub(crate) param_tys: Vec<ATyId>,
    pub(crate) result_ty_opt: Option<ATyId>,
}

pub(crate) enum ATy {
    Name(ANameId),
    App(ANameId, ATyIds),
    InferTy,
    Never,
    Unit,
    Ptr(APtrTy),
    Fn(AFnTy),
}

// -----------------------------------------------
// パターン
// -----------------------------------------------

pub(crate) struct APatTag;
pub(crate) type APatId = VecArenaId<APatTag>;
pub(crate) type APatArena = VecArena<APatTag, APat>;

pub(crate) struct AFieldPat {
    pub(crate) name: ANameId,
    pub(crate) pat_opt: Option<APatId>,
}

pub(crate) struct ARecordPat {
    pub(crate) left: ANameId,
    pub(crate) fields: Vec<AFieldPat>,
}

pub(crate) enum APat {
    Unit,
    True(PToken),
    False(PToken),
    Number(PToken),
    Char(PToken),
    Str(PToken),
    Wildcard(PToken),
    Name(ANameId),
    Record(ARecordPat),
}

// -----------------------------------------------
// 式
// -----------------------------------------------

pub(crate) struct AExprTag;
pub(crate) type AExprId = VecArenaId<AExprTag>;
pub(crate) type AExprIds = VecArenaSlice<AExprTag>;
pub(crate) type AExprArena = VecArena<AExprTag, AExpr>;

/// `name::[ty...]`
pub(crate) struct ATyAppExpr {
    pub(crate) left: ANameId,
    pub(crate) ty_args: ATyIds,
}

pub(crate) struct AFieldExpr {
    pub(crate) left: AExprId,
    pub(crate) field_opt: Option<PToken>,
}

/// `f()` or `a[]`
pub(crate) struct ACallLikeExpr {
    pub(crate) left: AExprId,
    pub(crate) args: AExprIds,
}

pub(crate) struct ACastExpr {
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

pub(crate) struct ALabeledArg {
    pub(crate) field_name: ANameId,
    pub(crate) value_opt: Option<AExprId>,
}

pub(crate) struct ARecordExpr {
    pub(crate) left: ANameId,
    pub(crate) ty_args_opt: Option<ATyIds>,
    pub(crate) fields: Vec<ALabeledArg>,
}

pub(crate) struct AJumpExpr {
    pub(crate) arg_opt: Option<AExprId>,
}

pub(crate) struct ABlockExpr {
    pub(crate) stmts: AStmtIds,
}

pub(crate) struct AIfExpr {
    pub(crate) cond_opt: Option<AExprId>,
    pub(crate) body_opt: Option<AExprId>,
    pub(crate) alt_opt: Option<AExprId>,
}

pub(crate) struct AArm {
    pub(crate) pat_opt: Option<APatId>,
    pub(crate) body_opt: Option<AExprId>,
    pub(crate) loc: PLoc,
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
    Unit,
    True,
    False,
    Number(PToken),
    Char(PToken),
    Str(PToken),
    Name(ANameId),
    TyApp(ATyAppExpr),
    Record(ARecordExpr),
    Field(AFieldExpr),
    Call(ACallLikeExpr),
    Index(ACallLikeExpr),
    Cast(ACastExpr),
    UnaryOp(AUnaryOpExpr),
    BinaryOp(ABinaryOpExpr),
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
// 文
// -----------------------------------------------

pub(crate) struct AStmtTag;
pub(crate) type AStmtId = VecArenaId<AStmtTag>;
pub(crate) type AStmtIds = VecArenaSlice<AStmtTag>;
pub(crate) type AStmtArena = VecArena<AStmtTag, AStmt>;

#[derive(Default)]
pub(crate) struct AStmtModifiers {
    pub(crate) vis_opt: Option<KVis>,
    // attrs: Vec<AAttr>,
}

/// let, const, static, const variant, field of record variant
pub(crate) struct AFieldLikeDecl {
    pub(crate) modifiers: AStmtModifiers,
    pub(crate) name_opt: Option<ANameId>,
    pub(crate) ty_opt: Option<ATyId>,
    pub(crate) value_opt: Option<AExprId>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct AFieldDeclKey {
    parent: AVariantDeclKey,
    index: usize,
}

impl AFieldDeclKey {
    pub(crate) fn new(parent: AVariantDeclKey, index: usize) -> Self {
        Self { parent, index }
    }

    pub(crate) fn element(self, tree: &PTree) -> PElement {
        // OK: インデックスが与えられているので、要素は存在するはず
        self.parent
            .element(tree)
            .of(&tree.elements)
            .nth_child_element_of(PElementKind::FieldDecl, self.index, tree)
            .unwrap()
    }
}

pub(crate) struct AExprStmt {
    pub(crate) expr: AExprId,
}

pub(crate) struct AParamDecl {
    pub(crate) name: ANameId,
    pub(crate) ty_opt: Option<ATyId>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct AParamDeclKey {
    parent: AStmtId,
    index: usize,
}

impl AParamDeclKey {
    pub(crate) fn new(parent: AStmtId, index: usize) -> Self {
        Self { parent, index }
    }

    pub(crate) fn element(self, tree: &PTree) -> PElement {
        // OK: インデックスが与えられているので、要素は存在するはず
        self.parent
            .element(tree)
            .of(&tree.elements)
            .nth_child_element_of(PElementKind::ParamDecl, self.index, tree)
            .unwrap()
    }
}

pub(crate) struct AFnLikeStmt {
    pub(crate) modifiers: AStmtModifiers,
    pub(crate) name_opt: Option<ANameId>,
    pub(crate) ty_params: Vec<ATyParamDecl>,
    pub(crate) params: Vec<AParamDecl>,
    pub(crate) result_ty_opt: Option<ATyId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct ARecordVariantDecl {
    pub(crate) name: ANameId,
    pub(crate) ty_params: Vec<ATyParamDecl>,
    pub(crate) fields: Vec<AFieldLikeDecl>,
}

pub(crate) enum AVariantDecl {
    Const(AFieldLikeDecl),
    Record(ARecordVariantDecl),
}

impl AVariantDecl {
    pub(crate) fn name_opt(&self) -> Option<ANameId> {
        match self {
            AVariantDecl::Const(decl) => decl.name_opt,
            AVariantDecl::Record(decl) => Some(decl.name),
        }
    }

    pub(crate) fn as_const(&self) -> Option<&AFieldLikeDecl> {
        match self {
            AVariantDecl::Const(it) => Some(it),
            _ => None,
        }
    }

    pub(crate) fn is_unit_like(&self) -> bool {
        match self {
            AVariantDecl::Const(decl) => decl.ty_opt.is_none() && decl.value_opt.is_none(),
            AVariantDecl::Record(_) => false,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum AVariantDeclKey {
    Enum(AStmtId, usize),
    Struct(AStmtId),
}

impl AVariantDeclKey {
    pub(crate) fn loc(self) -> PLoc {
        PLoc::VariantDecl(self)
    }

    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let (parent, index) = match self {
            AVariantDeclKey::Enum(parent, index) => (parent, index),
            AVariantDeclKey::Struct(parent) => (parent, 0),
        };

        let ty_decl = parent.element(tree);
        ty_decl
            .of(&tree.elements)
            .nth_child_element_either_of(PElementKind::VARIANT_DECL, index, tree)
            .unwrap_or(ty_decl)
    }
}

pub(crate) struct AEnumStmt {
    pub(crate) modifiers: AStmtModifiers,
    pub(crate) name_opt: Option<ANameId>,
    pub(crate) variants: Vec<AVariantDecl>,
}

impl AEnumStmt {
    pub(crate) fn as_const_enum(&self) -> Option<Vec<&AFieldLikeDecl>> {
        self.variants.iter().map(AVariantDecl::as_const).collect()
    }
}

pub(crate) struct AStructStmt {
    pub(crate) modifiers: AStmtModifiers,
    pub(crate) variant_opt: Option<AVariantDecl>,
}

impl AStructStmt {
    pub(crate) fn name_opt(&self) -> Option<ANameId> {
        self.variant_opt.as_ref()?.name_opt()
    }
}

pub(crate) struct AUseStmt {
    pub(crate) modifiers: AStmtModifiers,
    pub(crate) name_opt: Option<ANameId>,
}

pub(crate) enum AStmt {
    Attr,
    Expr(AExprId),
    Let(AFieldLikeDecl),
    Const(AFieldLikeDecl),
    Static(AFieldLikeDecl),
    Fn(AFnLikeStmt),
    ExternFn(AFnLikeStmt),
    Enum(AEnumStmt),
    Struct(AStructStmt),
    Use(AUseStmt),
}

impl AStmt {
    pub(crate) fn name_opt(&self) -> Option<ANameId> {
        match self {
            AStmt::Attr | AStmt::Expr(_) => None,
            AStmt::Let(stmt) => stmt.name_opt,
            AStmt::Const(stmt) => stmt.name_opt,
            AStmt::Static(stmt) => stmt.name_opt,
            AStmt::Fn(stmt) => stmt.name_opt,
            AStmt::ExternFn(stmt) => stmt.name_opt,
            AStmt::Enum(stmt) => stmt.name_opt,
            AStmt::Struct(stmt) => stmt.name_opt(),
            AStmt::Use(stmt) => stmt.name_opt,
        }
    }
}

// -----------------------------------------------
// 構文要素
// -----------------------------------------------

impl ANameId {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let event_id = self.of(&tree.ast.name_events).id();
        event_id.of(&tree.ast.events).unwrap()
    }

    pub(crate) fn element_data(self, tree: &PTree) -> &PElementData {
        self.element(tree).of(&tree.elements)
    }

    pub(crate) fn loc(self) -> PLoc {
        PLoc::Name(self)
    }
}

impl ATyId {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let event_id = self.of(&tree.ast.ty_events).id();
        event_id.of(&tree.ast.events).unwrap()
    }

    pub(crate) fn element_data(self, tree: &PTree) -> &PElementData {
        self.element(tree).of(&tree.elements)
    }

    pub(crate) fn loc(self) -> PLoc {
        PLoc::Ty(self)
    }
}

impl APatId {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let event_id = self.of(&tree.ast.pat_events).id();
        event_id.of(&tree.ast.events).unwrap()
    }

    pub(crate) fn element_data(self, tree: &PTree) -> &PElementData {
        self.element(tree).of(&tree.elements)
    }

    pub(crate) fn loc(self) -> PLoc {
        PLoc::Pat(self)
    }
}

impl AExprId {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let event_id = self.of(&tree.ast.expr_events).id();
        event_id.of(&tree.ast.events).unwrap()
    }

    pub(crate) fn element_data(self, tree: &PTree) -> &PElementData {
        self.element(tree).of(&tree.elements)
    }

    pub(crate) fn loc(self) -> PLoc {
        PLoc::Expr(self)
    }
}

impl AStmtId {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let event_id = self.of(&tree.ast.stmt_events).id();
        event_id.of(&tree.ast.events).unwrap()
    }

    pub(crate) fn element_data(self, tree: &PTree) -> &PElementData {
        self.element(tree).of(&tree.elements)
    }

    pub(crate) fn loc(self, tree: &PTree) -> PLoc {
        PLoc::Stmt(self)
    }
}

#[derive(Copy, Clone)]
pub(crate) enum AElementId {
    Ty(ATyId),
    Pat(APatId),
    Expr(AExprId),
    Stmt(AStmtId),
}

#[derive(Copy, Clone)]
pub(crate) enum AElementRef<'a> {
    Ty(&'a ATy),
    Pat(&'a APat),
    Expr(&'a AExpr),
    Stmt(&'a AStmt),
}

impl AElementId {
    fn of<'a>(self, ast: &'a ATree) -> AElementRef<'a> {
        match self {
            AElementId::Ty(ty) => AElementRef::Ty(ty.of(&ast.tys)),
            AElementId::Pat(pat) => AElementRef::Pat(pat.of(&ast.pats)),
            AElementId::Expr(expr) => AElementRef::Expr(expr.of(&ast.exprs)),
            AElementId::Stmt(stmt) => AElementRef::Stmt(stmt.of(&ast.stmts)),
        }
    }

    fn event_id(self, tree: &PTree) -> EventId {
        match self {
            AElementId::Ty(ty) => ty.of(&tree.ast.ty_events).id(),
            AElementId::Pat(pat) => pat.of(&tree.ast.pat_events).id(),
            AElementId::Expr(expr) => expr.of(&tree.ast.expr_events).id(),
            AElementId::Stmt(stmt) => stmt.of(&tree.ast.stmt_events).id(),
        }
    }

    fn untyped(self, tree: &PTree) -> PElement {
        let ast = &tree.ast;
        self.event_id(tree).of(&ast.events).unwrap()
    }

    fn as_untyped(self, tree: &PTree) -> &PElementData {
        self.untyped(tree).of(&tree.elements)
    }
}

// -----------------------------------------------
// ルート
// -----------------------------------------------

#[derive(Default)]
pub(crate) struct ARoot {
    pub(crate) stmts: AStmtIds,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum ALoc {
    Ty(ATyId),
    Pat(APatId),
    Expr(AExprId),
    Stmt(AStmtId),
}

#[derive(Default)]
pub(crate) struct ATree {
    pub(super) root: ARoot,
    pub(super) names: ANameArena,
    pub(super) tys: ATyArena,
    pub(super) pats: APatArena,
    pub(super) exprs: AExprArena,
    pub(super) stmts: AStmtArena,
    pub(super) name_events: VecArena<ANameTag, NameEnd>,
    pub(super) ty_events: VecArena<ATyTag, TyEnd>,
    pub(super) pat_events: VecArena<APatTag, PatEnd>,
    pub(super) expr_events: VecArena<AExprTag, ExprEnd>,
    pub(super) stmt_events: VecArena<AStmtTag, StmtEnd>,
    pub(super) events: EventArena,
}

impl ATree {
    pub(crate) fn root_stmts(&self) -> AStmtIds {
        self.root.stmts.clone()
    }

    pub(crate) fn names(&self) -> &ANameArena {
        &self.names
    }

    pub(crate) fn tys(&self) -> &ATyArena {
        &self.tys
    }

    pub(crate) fn pats(&self) -> &APatArena {
        &self.pats
    }

    pub(crate) fn exprs(&self) -> &AExprArena {
        &self.exprs
    }

    pub(crate) fn stmts(&self) -> &AStmtArena {
        &self.stmts
    }
}
