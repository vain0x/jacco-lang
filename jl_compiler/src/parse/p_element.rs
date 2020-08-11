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
    DiscardPat,
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
    pub(crate) fn range(self, tree: &PTree) -> Result<TRange, &'static str> {
        self.of(&tree.elements).range(tree)
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

    pub(crate) fn first_token(&self, tree: &PTree) -> Option<PToken> {
        self.children()
            .iter()
            .find_map(|node| node.first_token(tree))
    }

    pub(crate) fn last_token(&self, tree: &PTree) -> Option<PToken> {
        self.children()
            .iter()
            .rev()
            .find_map(|node| node.last_token(tree))
    }

    pub(crate) fn range(&self, tree: &PTree) -> Result<TRange, &'static str> {
        let range = match (self.first_token(tree), self.last_token(tree)) {
            (Some(first), Some(last)) => first.range(&tree.tokens).join(last.range(&tree.tokens)),
            (Some(token), None) | (None, Some(token)) => token.range(&tree.tokens),
            (None, None) => return Err("<PElementData::range>"),
        };
        Ok(range)
    }

    #[allow(unused)]
    pub(crate) fn nth_child_token_of(
        &self,
        kind: TokenKind,
        index: usize,
        tree: &PTree,
    ) -> Option<PToken> {
        self.children()
            .iter()
            .copied()
            .filter_map(PNode::as_token)
            .filter(|token| token.of(&tree.tokens).kind() == kind)
            .nth(index)
    }

    pub(crate) fn nth_child_element_of(
        &self,
        kind: PElementKind,
        index: usize,
        tree: &PTree,
    ) -> Option<PElement> {
        self.children()
            .iter()
            .copied()
            .filter_map(PNode::as_element)
            .filter(|element| element.of(&tree.elements).kind() == kind)
            .nth(index)
    }

    pub(crate) fn nth_child_element_either_of(
        &self,
        kinds: &[PElementKind],
        index: usize,
        tree: &PTree,
    ) -> Option<PElement> {
        self.children()
            .iter()
            .copied()
            .filter_map(PNode::as_element)
            .filter(|element| kinds.contains(&element.of(&tree.elements).kind()))
            .nth(index)
    }
}
