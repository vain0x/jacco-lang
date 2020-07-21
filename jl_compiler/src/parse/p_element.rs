use super::*;
use crate::utils::{VecArena, VecArenaId};
use std::fmt::Debug;

/// 構文要素の種類
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum PElementKind {
    Name,
    #[allow(unused)]
    Param,
    #[allow(unused)]
    Arg,
    Ty,
    Pat,
    Expr,
    Decl,
    Root,
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
