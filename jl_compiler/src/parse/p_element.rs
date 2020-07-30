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

pub(crate) struct PElementTag;

/// 構文要素 (構文木の非終端ノード)
pub(crate) type PElement = VecArenaId<PElementTag>;

pub(crate) type PElementArena = VecArena<PElementTag, PElementData>;

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

    pub(crate) fn range(&self, root: &PRoot) -> TRange {
        match (self.first_token(root), self.last_token(root)) {
            (Some(first), Some(last)) => first
                .loc(&root.tokens)
                .range()
                .unite(&last.loc(&root.tokens).range()),
            (Some(token), None) | (None, Some(token)) => token.loc(&root.tokens).range(),
            (None, None) => TRange::ZERO,
        }
    }
}
