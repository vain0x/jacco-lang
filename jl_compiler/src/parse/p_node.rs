use super::*;
use std::fmt::{self, Debug, Formatter};

// -----------------------------------------------
// NodeKind
// -----------------------------------------------

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum PNodeKind {
    Token(TokenKind),
    Element(PElementKind),
}

impl Debug for PNodeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PNodeKind::Token(inner) => Debug::fmt(inner, f),
            PNodeKind::Element(inner) => Debug::fmt(inner, f),
        }
    }
}

// -----------------------------------------------
// PNode
// -----------------------------------------------

/// 構文木のノード (構文要素またはトークン) のID
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum PNode {
    Token(PToken),
    Element(PElement),
}

impl PNode {
    pub(crate) fn as_element(self) -> Option<PElement> {
        match self {
            PNode::Element(element) => Some(element),
            _ => None,
        }
    }

    pub(crate) fn of(self, root: &PRoot) -> PNodeRef<'_> {
        match self {
            PNode::Token(token) => PNodeRef::Token(token.of(&root.tokens)),
            PNode::Element(element) => PNodeRef::Element(element.of(&root.elements)),
        }
    }
}

impl From<TokenKind> for PNodeKind {
    fn from(kind: TokenKind) -> Self {
        PNodeKind::Token(kind)
    }
}

impl From<PElementKind> for PNodeKind {
    fn from(kind: PElementKind) -> Self {
        PNodeKind::Element(kind)
    }
}

impl Debug for PNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PNode::Token(inner) => Debug::fmt(inner, f),
            PNode::Element(inner) => Debug::fmt(inner, f),
        }
    }
}

// -----------------------------------------------
// PNodeRef
// -----------------------------------------------

#[derive(Copy, Clone)]
pub(crate) enum PNodeRef<'a> {
    Token(&'a TokenData),
    Element(&'a PElementData),
}

impl PNodeRef<'_> {
    #[allow(unused)]
    pub(crate) fn kind(&self) -> PNodeKind {
        match self {
            PNodeRef::Token(token) => PNodeKind::Token(token.kind()),
            PNodeRef::Element(element) => PNodeKind::Element(element.kind()),
        }
    }
}

impl<'a> From<&'a TokenData> for PNodeRef<'a> {
    fn from(token: &'a TokenData) -> Self {
        PNodeRef::Token(token)
    }
}

impl<'a> From<&'a PElementData> for PNodeRef<'a> {
    fn from(element: &'a PElementData) -> Self {
        PNodeRef::Element(element)
    }
}
