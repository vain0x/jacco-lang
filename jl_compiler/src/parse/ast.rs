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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum ANameKey {
    Pat(APatId),
    Expr(AExprId),
    /// static や fn などの名前。
    Decl(ADeclId),
    TyParam(ADeclId),
    Param(AParamDeclKey),
    Field(AFieldDeclKey),
    Variant(AVariantDeclKey),
    Id(ANameId),
}

impl ANameKey {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let parent = match self {
            ANameKey::Pat(pat_id) => pat_id.element(tree),
            ANameKey::Expr(expr_id) => expr_id.element(tree),
            ANameKey::Decl(decl_id) => decl_id.element(tree),
            ANameKey::TyParam(key) => key.element(tree),
            ANameKey::Param(key) => key.element(tree),
            ANameKey::Field(key) => key.element(tree),
            ANameKey::Variant(key) => key.element(tree),
            ANameKey::Id(key) => key.element(tree),
        };
        parent
            .of(&tree.elements)
            .nth_child_element_of(PElementKind::Name, 0, tree)
            .unwrap_or_else(|| {
                // FIXME: 名前があるべき位置を見つける？

                // struct K[T] {} の T を指すときは struct 宣言 → バリアント → 型パラメータ、と辿る必要がある。
                if let ANameKey::TyParam(_) = self {
                    if let Some(name) = parent
                        .of(&tree.elements)
                        .nth_child_element_either_of(PElementKind::VARIANT_DECL, 0, tree)
                        .and_then(|variant| {
                            variant.of(&tree.elements).nth_child_element_of(
                                PElementKind::Name,
                                0,
                                tree,
                            )
                        })
                    {
                        return name;
                    }
                }

                parent
            })
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
    pub(crate) fields: Vec<ALabeledArg>,
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

pub(crate) struct AExprDecl {
    pub(crate) expr: AExprId,
}

pub(crate) struct AParamDecl {
    pub(crate) name: ANameId,
    pub(crate) ty_opt: Option<ATyId>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct AParamDeclKey {
    parent: ADeclId,
    index: usize,
}

impl AParamDeclKey {
    pub(crate) fn new(parent: ADeclId, index: usize) -> Self {
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

pub(crate) struct AFnLikeDecl {
    pub(crate) modifiers: ADeclModifiers,
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
    Enum(ADeclId, usize),
    Struct(ADeclId),
}

impl AVariantDeclKey {
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

pub(crate) struct AEnumDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<ANameId>,
    pub(crate) variants: Vec<AVariantDecl>,
}

impl AEnumDecl {
    pub(crate) fn as_const_enum(&self) -> Option<Vec<&AFieldLikeDecl>> {
        self.variants.iter().map(AVariantDecl::as_const).collect()
    }
}

pub(crate) struct AStructDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) variant_opt: Option<AVariantDecl>,
}

impl AStructDecl {
    pub(crate) fn name_opt(&self) -> Option<ANameId> {
        self.variant_opt.as_ref()?.name_opt()
    }
}

pub(crate) struct AUseDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<ANameId>,
}

pub(crate) enum ADecl {
    Attr,
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

impl ADecl {
    pub(crate) fn name_opt(&self) -> Option<ANameId> {
        match self {
            ADecl::Attr | ADecl::Expr(_) => None,
            ADecl::Let(decl) => decl.name_opt,
            ADecl::Const(decl) => decl.name_opt,
            ADecl::Static(decl) => decl.name_opt,
            ADecl::Fn(decl) => decl.name_opt,
            ADecl::ExternFn(decl) => decl.name_opt,
            ADecl::Enum(decl) => decl.name_opt,
            ADecl::Struct(decl) => decl.name_opt(),
            ADecl::Use(decl) => decl.name_opt,
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
        PLoc::Name(ANameKey::Id(self))
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

impl ADeclId {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let event_id = self.of(&tree.ast.decl_events).id();
        event_id.of(&tree.ast.events).unwrap()
    }

    pub(crate) fn element_data(self, tree: &PTree) -> &PElementData {
        self.element(tree).of(&tree.elements)
    }

    pub(crate) fn loc(self, tree: &PTree) -> PLoc {
        PLoc::Decl(self)
    }
}

#[derive(Copy, Clone)]
pub(crate) enum AElementId {
    Ty(ATyId),
    Pat(APatId),
    Expr(AExprId),
    Decl(ADeclId),
}

#[derive(Copy, Clone)]
pub(crate) enum AElementRef<'a> {
    Ty(&'a ATy),
    Pat(&'a APat),
    Expr(&'a AExpr),
    Decl(&'a ADecl),
}

impl AElementId {
    fn of<'a>(self, ast: &'a ATree) -> AElementRef<'a> {
        match self {
            AElementId::Ty(ty) => AElementRef::Ty(ty.of(&ast.tys)),
            AElementId::Pat(pat) => AElementRef::Pat(pat.of(&ast.pats)),
            AElementId::Expr(expr) => AElementRef::Expr(expr.of(&ast.exprs)),
            AElementId::Decl(decl) => AElementRef::Decl(decl.of(&ast.decls)),
        }
    }

    fn event_id(self, tree: &PTree) -> EventId {
        match self {
            AElementId::Ty(ty) => ty.of(&tree.ast.ty_events).id(),
            AElementId::Pat(pat) => pat.of(&tree.ast.pat_events).id(),
            AElementId::Expr(expr) => expr.of(&tree.ast.expr_events).id(),
            AElementId::Decl(decl) => decl.of(&tree.ast.decl_events).id(),
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
    pub(crate) decls: ADeclIds,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum ALoc {
    Ty(ATyId),
    Pat(APatId),
    Expr(AExprId),
    Decl(ADeclId),
}

#[derive(Default)]
pub(crate) struct ATree {
    pub(super) root: ARoot,
    pub(super) names: ANameArena,
    pub(super) tys: ATyArena,
    pub(super) pats: APatArena,
    pub(super) exprs: AExprArena,
    pub(super) decls: ADeclArena,
    pub(super) name_events: VecArena<ANameTag, NameEnd>,
    pub(super) ty_events: VecArena<ATyTag, TyEnd>,
    pub(super) pat_events: VecArena<APatTag, PatEnd>,
    pub(super) expr_events: VecArena<AExprTag, ExprEnd>,
    pub(super) decl_events: VecArena<ADeclTag, DeclEnd>,
    pub(super) events: EventArena,
}

impl ATree {
    pub(crate) fn root_decls(&self) -> ADeclIds {
        self.root.decls.clone()
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

    pub(crate) fn decls(&self) -> &ADeclArena {
        &self.decls
    }
}
