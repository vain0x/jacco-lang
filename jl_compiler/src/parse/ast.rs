//! 抽象構文木 (abstract syntax tree; AST)

#![allow(unused)]

use super::{
    p_token::PTokenSlice, DeclEnd, EventArena, EventId, ExprEnd, PBinaryOp, PElement,
    PElementArena, PElementData, PElementKind, PLoc, PToken, PTree, PUnaryOp, PatEnd, TyEnd,
};
use crate::{
    cps::{KMut, KVis},
    token::TokenKind,
    utils::{VecArena, VecArenaId, VecArenaSlice},
};
use std::fmt::{self, Debug, Formatter};

// -----------------------------------------------
// 名前
// -----------------------------------------------

pub(crate) struct AName {
    pub(crate) text: String,
    pub(crate) full_name: String,
}

impl AName {
    pub(crate) fn root_text(&self) -> &str {
        self.full_name.split("::").next().unwrap_or("")
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum ANameKey {
    Ty(ATyId),
    Pat(APatId),
    Expr(AExprId),
    /// static や fn などの名前。
    Decl(ADeclId),
    Param(AParamDeclKey),
    Field(AFieldDeclKey),
    Variant(AVariantDeclKey),
}

impl ANameKey {
    pub(crate) fn element(self, tree: &PTree) -> PElement {
        let parent = match self {
            ANameKey::Ty(ty_id) => ty_id.element(tree),
            ANameKey::Pat(pat_id) => pat_id.element(tree),
            ANameKey::Expr(expr_id) => expr_id.element(tree),
            ANameKey::Decl(decl_id) => decl_id.element(tree),
            ANameKey::Param(key) => key.element(tree),
            ANameKey::Field(key) => key.element(tree),
            ANameKey::Variant(key) => key.element(tree),
        };
        parent
            .of(&tree.elements)
            .nth_child_element_of(PElementKind::Name, 0, tree)
            .unwrap()
    }
}

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
    InferTy,
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
    pub(crate) name: AName,
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

pub(crate) struct AFieldExpr {
    pub(crate) field_name: AName,
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
    pub(crate) name_opt: Option<AName>,
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
    pub(crate) name: AName,
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
        self.parent
            .element(tree)
            .of(&tree.elements)
            .nth_child_element_of(PElementKind::ParamDecl, self.index, tree)
            .unwrap()
    }
}

pub(crate) struct AFnLikeDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<AName>,
    pub(crate) params: Vec<AParamDecl>,
    pub(crate) result_ty_opt: Option<ATyId>,
    pub(crate) body_opt: Option<AExprId>,
}

pub(crate) struct ARecordVariantDecl {
    pub(crate) name: AName,
    pub(crate) fields: Vec<AFieldLikeDecl>,
}

pub(crate) enum AVariantDecl {
    Const(AFieldLikeDecl),
    Record(ARecordVariantDecl),
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

        parent
            .element(tree)
            .of(&tree.elements)
            .nth_child_element_either_of(PElementKind::VARIANT_DECL, index, tree)
            .unwrap()
    }
}

pub(crate) struct AEnumDecl {
    pub(crate) modifiers: ADeclModifiers,
    pub(crate) name_opt: Option<AName>,
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

// -----------------------------------------------
// 構文要素
// -----------------------------------------------

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
    pub(super) tys: ATyArena,
    pub(super) pats: APatArena,
    pub(super) exprs: AExprArena,
    pub(super) decls: ADeclArena,
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

// -----------------------------------------------
// ダンプ
// -----------------------------------------------

#[derive(Copy, Clone)]
struct Render<'a> {
    element: AElementId,
    tree: &'a super::PTree,
}

impl<'a> Render<'a> {
    fn with_ty(self, ty: ATyId) -> Self {
        Self {
            element: AElementId::Ty(ty),
            ..self
        }
    }

    fn with_pat(self, pat: APatId) -> Self {
        Self {
            element: AElementId::Pat(pat),
            ..self
        }
    }

    fn with_expr(self, expr: AExprId) -> Self {
        Self {
            element: AElementId::Expr(expr),
            ..self
        }
    }

    fn with_decl(self, decl: ADeclId) -> Self {
        Self {
            element: AElementId::Decl(decl),
            ..self
        }
    }

    fn element(self) -> PElement {
        let ast = &self.tree.ast;
        let event_id = match self.element {
            AElementId::Ty(ty) => ty.of(&ast.ty_events).id(),
            AElementId::Pat(pat) => pat.of(&ast.pat_events).id(),
            AElementId::Expr(expr) => expr.of(&ast.expr_events).id(),
            AElementId::Decl(decl) => decl.of(&ast.decl_events).id(),
        };
        event_id.of(&self.tree.ast.events).unwrap()
    }
}

fn go(element: AElementId, tree: &PTree, f: &mut Formatter<'_>) -> fmt::Result {
    for node in element.as_untyped(tree).children() {
        match node {
            super::PNode::Token(token) => {
                write!(f, "{}", token.text(&tree.tokens))?;
            }
            super::PNode::Element(element) => {
                write!(f, "<{:?}>", element.of(&tree.elements).kind())?
            }
        }
    }
    Ok(())
}

impl<'a> Debug for Render<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ast = &self.tree.ast;
        match self.element {
            AElementId::Ty(ty) => match ty.of(&self.tree.ast.tys) {
                ATy::Name(_) => go(self.element, self.tree, f),
                ATy::InferTy => write!(f, "_"),
                ATy::Never => write!(f, "!"),
                ATy::Unit => write!(f, "()"),
                ATy::Ptr(APtrTy { mut_opt, ty_opt }) => {
                    write!(f, "*")?;

                    if let Some(KMut::Mut) = mut_opt {
                        write!(f, "mut ")?;
                    }

                    match *ty_opt {
                        Some(ty) => Debug::fmt(&self.with_ty(ty), f),
                        None => write!(f, "{{missing}}"),
                    }
                }
            },
            AElementId::Pat(pat) => match pat.of(&self.tree.ast.pats) {
                APat::Number(_)
                | APat::Char(_)
                | APat::Str(_)
                | APat::True(_)
                | APat::False(_)
                | APat::Discard(_)
                | APat::Name(_)
                | APat::Unit => go(self.element, self.tree, f),
                APat::Record(ARecordPat { .. }) => write!(f, "??? {{ .. }}"),
            },
            AElementId::Expr(expr) => match expr.of(&self.tree.ast.exprs) {
                AExpr::Number(_)
                | AExpr::Char(_)
                | AExpr::Str(_)
                | AExpr::True
                | AExpr::False
                | AExpr::Name(_) => go(self.element, self.tree, f),
                AExpr::Unit => write!(f, "()"),
                AExpr::Record(_) | AExpr::DotField(_) => go(self.element, self.tree, f),
                AExpr::Call(ACallLikeExpr { left, args }) => {
                    self.with_expr(*left).fmt(f)?;

                    let mut tuple = f.debug_tuple("");
                    for arg in args.iter() {
                        tuple.field(&self.with_expr(arg));
                    }
                    tuple.finish()
                }
                AExpr::Index(_) => write!(f, ".."),
                AExpr::As(_) => write!(f, ".."),
                AExpr::UnaryOp(AUnaryOpExpr {
                    op,
                    mut_opt,
                    arg_opt,
                }) => {
                    write!(f, "{:?}", op)?;

                    if let Some(KMut::Mut) = mut_opt {
                        write!(f, "mut ")?;
                    }

                    match arg_opt {
                        Some(arg) => Debug::fmt(&self.with_expr(*arg), f),
                        None => write!(f, "{{missing}}"),
                    }
                }
                AExpr::BinaryOp(ABinaryOpExpr {
                    op,
                    left,
                    right_opt,
                }) => {
                    Debug::fmt(&self.with_expr(*left), f)?;

                    write!(f, "{:?}", op)?;

                    match right_opt {
                        Some(right) => Debug::fmt(&self.with_expr(*right), f),
                        None => write!(f, "{{missing}}"),
                    }
                }
                AExpr::Block(ABlockExpr { decls }) => f
                    .debug_list()
                    .entries(decls.iter().map(|decl| self.with_decl(decl)))
                    .finish(),
                AExpr::Break(_)
                | AExpr::Continue
                | AExpr::Return(_)
                | AExpr::If(_)
                | AExpr::Match(_)
                | AExpr::While(_)
                | AExpr::Loop(_) => go(self.element, self.tree, f),
            },
            AElementId::Decl(decl) => match decl.of(&self.tree.ast.decls) {
                ADecl::Expr(expr) => Debug::fmt(&self.with_expr(*expr), f),
                ADecl::Attr | ADecl::Let(_) | ADecl::Const(_) | ADecl::Static(_) => {
                    go(self.element, self.tree, f)
                }
                ADecl::Fn(AFnLikeDecl {
                    name_opt,
                    params,
                    result_ty_opt,
                    body_opt,
                    ..
                }) => {
                    write!(f, "fn ")?;

                    let mut tuple = f.debug_tuple("");
                    for param in params {
                        tuple.field(&"_");
                    }
                    tuple.finish()?;

                    if let Some(body) = body_opt {
                        Debug::fmt(&self.with_expr(*body), f)?;
                    }
                    Ok(())
                }
                ADecl::ExternFn(_) | ADecl::Enum(_) | ADecl::Struct(_) | ADecl::Use(_) => {
                    go(self.element, self.tree, f)
                }
            },
        }
    }
}

struct RenderRoot<'a> {
    tree: &'a PTree,
}

impl Debug for RenderRoot<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.tree.ast.root.decls.iter().map(|decl| Render {
                element: AElementId::Decl(decl),
                tree: self.tree,
            }))
            .finish()
    }
}

pub(crate) fn dump_ast(tree: &super::PTree) {
    log::trace!(
        "exprs {}, decls {}, root_decls {}",
        tree.ast.exprs.len(),
        tree.ast.decls.len(),
        tree.ast.root.decls.len()
    );

    #[cfg(skip)]
    log::trace!("AST = {:#?}", RenderRoot { tree });
}
