use super::*;
use crate::{
    source::TRange,
    utils::{VecArena, VecArenaId},
};
use std::fmt::Debug;

/// 構文要素の種類
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum PElementKind {
    Name,

    // 型
    NameTy,
    NeverTy,
    UnitTy,
    PtrTy,

    // パターン
    CharPat,
    NamePat,
    RecordPat,

    // 式の一部
    FieldExpr,
    Arg,
    Arm,

    // 式
    NumberExpr,
    CharExpr,
    StrExpr,
    TrueExpr,
    FalseExpr,
    NameExpr,
    UnitExpr,
    GroupExpr,
    RecordExpr,
    DotFieldExpr,
    CallExpr,
    IndexExpr,
    AsExpr,
    UnaryOpExpr,
    BinaryOpExpr,
    BlockExpr,
    BreakExpr,
    ContinueExpr,
    ReturnExpr,
    IfExpr,
    MatchExpr,
    WhileExpr,
    LoopExpr,

    // 宣言の一部
    ParamDecl,
    ConstVariantDecl,
    FieldDecl,
    RecordVariantDecl,

    // 宣言
    AttrDecl,
    ExprDecl,
    LetDecl,
    ConstDecl,
    StaticDecl,
    FnDecl,
    ExternFnDecl,
    EnumDecl,
    StructDecl,
    UseDecl,
    RootDecl,
}

impl PElementKind {
    pub(crate) const VARIANT_DECL: &'static [PElementKind] = &[
        PElementKind::ConstVariantDecl,
        PElementKind::RecordVariantDecl,
    ];
}

pub(crate) struct PElementTag;

/// 構文要素 (構文木の非終端ノード)
pub(crate) type PElement = VecArenaId<PElementTag>;

pub(crate) type PElementArena = VecArena<PElementTag, PElementData>;

impl PElement {
    pub(crate) fn range(self, root: &PRoot) -> Result<TRange, &'static str> {
        self.of(&root.elements).range(root)
    }
}

/// 構文要素のデータ
#[derive(Clone, Debug)]
pub(crate) struct PElementData {
    kind: PElementKind,
    children: Vec<PNode>,
}

impl PElementData {
    pub(crate) fn new(kind: PElementKind, children: Vec<PNode>) -> Self {
        PElementData { kind, children }
    }

    #[allow(unused)]
    pub(crate) fn kind(&self) -> PElementKind {
        self.kind
    }

    #[allow(unused)]
    pub(crate) fn children(&self) -> &[PNode] {
        &self.children
    }

    pub(crate) fn first_token(&self, root: &PRoot) -> Option<PToken> {
        self.children()
            .iter()
            .find_map(|node| node.first_token(root))
    }

    pub(crate) fn last_token(&self, root: &PRoot) -> Option<PToken> {
        self.children()
            .iter()
            .rev()
            .find_map(|node| node.last_token(root))
    }

    pub(crate) fn range(&self, root: &PRoot) -> Result<TRange, &'static str> {
        let range = match (self.first_token(root), self.last_token(root)) {
            (Some(first), Some(last)) => first.range(&root.tokens)?.join(last.range(&root.tokens)?),
            (Some(token), None) | (None, Some(token)) => token.range(&root.tokens)?,
            (None, None) => return Err("<PElementData::range>"),
        };
        Ok(range)
    }

    #[allow(unused)]
    pub(crate) fn nth_child_token_of(
        &self,
        kind: TokenKind,
        index: usize,
        root: &PRoot,
    ) -> Option<PToken> {
        self.children()
            .iter()
            .copied()
            .filter_map(PNode::as_token)
            .filter(|token| token.of(&root.tokens).kind() == kind)
            .nth(index)
    }

    pub(crate) fn nth_child_element_of(
        &self,
        kind: PElementKind,
        index: usize,
        root: &PRoot,
    ) -> Option<PElement> {
        self.children()
            .iter()
            .copied()
            .filter_map(PNode::as_element)
            .filter(|element| element.of(&root.elements).kind() == kind)
            .nth(index)
    }

    pub(crate) fn nth_child_element_either_of(
        &self,
        kinds: &[PElementKind],
        index: usize,
        root: &PRoot,
    ) -> Option<PElement> {
        self.children()
            .iter()
            .copied()
            .filter_map(PNode::as_element)
            .filter(|element| kinds.contains(&element.of(&root.elements).kind()))
            .nth(index)
    }
}
