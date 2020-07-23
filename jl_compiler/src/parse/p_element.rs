use super::*;
use crate::utils::{VecArena, VecArenaId};
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
    PipeExpr,
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
    pub(crate) fn new(kind: PElementKind) -> Self {
        PElementData {
            kind,
            children: vec![],
        }
    }

    #[allow(unused)]
    pub(crate) fn kind(&self) -> PElementKind {
        self.kind
    }

    #[allow(unused)]
    pub(crate) fn children(&self) -> &[PNode] {
        &self.children
    }

    pub(crate) fn children_mut(&mut self) -> &mut Vec<PNode> {
        &mut self.children
    }
}
