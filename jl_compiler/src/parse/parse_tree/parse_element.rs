use super::*;

/// 構文要素 (トークンまたはノード) への不変参照
#[derive(Clone, Copy)]
pub(crate) enum PElementRef<'a> {
    Token(&'a TokenData),
    Node(&'a dyn PNode),
}

impl<'a> PElementRef<'a> {
    pub(crate) fn first_token(self) -> Option<&'a TokenData> {
        match self {
            PElementRef::Token(token) => Some(token),
            PElementRef::Node(node) => node.first_token(),
        }
    }

    pub(crate) fn last_token(self) -> Option<&'a TokenData> {
        match self {
            PElementRef::Token(token) => Some(token),
            PElementRef::Node(node) => node.last_token(),
        }
    }

    pub(crate) fn location(&self) -> Location {
        match self {
            PElementRef::Token(token) => token.location().clone(),
            PElementRef::Node(node) => node.location(),
        }
    }
}

impl<'a> From<&'a TokenData> for PElementRef<'a> {
    fn from(token: &TokenData) -> PElementRef {
        PElementRef::Token(token)
    }
}

impl<'a, N: PNode> From<&'a N> for PElementRef<'a> {
    fn from(node: &'a N) -> PElementRef<'a> {
        PElementRef::Node(node)
    }
}

/// 構文要素 (トークンまたはノード) への可変参照
pub(crate) enum PElementMut<'a> {
    Token(&'a mut TokenData),
    Node(&'a mut dyn PNode),
}

impl<'a> From<&'a mut TokenData> for PElementMut<'a> {
    fn from(token: &'a mut TokenData) -> PElementMut<'a> {
        PElementMut::Token(token)
    }
}

impl<'a, N: PNode> From<&'a mut N> for PElementMut<'a> {
    fn from(node: &'a mut N) -> PElementMut<'a> {
        PElementMut::Node(node)
    }
}

impl<'a> From<PElementMut<'a>> for PElementRef<'a> {
    fn from(element: PElementMut<'a>) -> PElementRef<'a> {
        match element {
            PElementMut::Token(token) => PElementRef::Token(token),
            PElementMut::Node(node) => PElementRef::Node(node),
        }
    }
}
