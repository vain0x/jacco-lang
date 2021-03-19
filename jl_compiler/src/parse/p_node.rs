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
#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum PNode {
    Token(PToken),
    Element(PElement),
}

impl PNode {
    #[cfg(unused)]
    pub(crate) fn as_token(self) -> Option<PToken> {
        match self {
            PNode::Token(token) => Some(token),
            _ => None,
        }
    }

    pub(crate) fn as_element(self) -> Option<PElement> {
        match self {
            PNode::Element(element) => Some(element),
            _ => None,
        }
    }

    #[cfg(unused)]
    pub(crate) fn of(self, tree: &PTree) -> PNodeRef<'_> {
        match self {
            PNode::Token(token) => PNodeRef::Token(token.of(&tree.tokens)),
            PNode::Element(element) => PNodeRef::Element(element.of(&tree.elements)),
        }
    }

    pub(crate) fn first_token(self, tree: &PTree) -> Option<PToken> {
        match self {
            PNode::Token(token) => Some(token),
            PNode::Element(element) => element.of(&tree.elements).first_token(tree),
        }
    }

    pub(crate) fn last_token(self, tree: &PTree) -> Option<PToken> {
        match self {
            PNode::Token(token) => Some(token),
            PNode::Element(element) => element.of(&tree.elements).last_token(tree),
        }
    }

    #[cfg(unused)]
    pub(crate) fn range(self, tree: &PTree) -> Result<TRange, &'static str> {
        match self {
            PNode::Token(token) => Ok(token.range(&tree.tokens)),
            PNode::Element(element) => element.of(&tree.elements).range(tree),
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
    #[cfg(unused)]
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
