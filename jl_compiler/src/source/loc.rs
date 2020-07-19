use super::{Doc, TRange};
use crate::parse::PToken;
use std::{
    fmt::{self, Debug, Formatter},
    path::Path,
};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum LocPart {
    Range,
    Behind,
}

impl LocPart {
    pub(crate) fn apply(self, range: TRange) -> TRange {
        match self {
            LocPart::Range => range,
            LocPart::Behind => range.behind(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum Loc {
    Range {
        doc: Doc,
        range: TRange,
    },
    Token {
        doc: Doc,
        token: PToken,
    },
    TokenBehind {
        doc: Doc,
        token: PToken,
    },
    TokenRange {
        doc: Doc,
        first: PToken,
        last: PToken,
    },
}

impl Loc {
    pub(crate) fn doc(self) -> Doc {
        match self {
            Loc::Range { doc, .. } => doc,
            Loc::Token { doc, .. } => doc,
            Loc::TokenBehind { doc, .. } => doc,
            Loc::TokenRange { doc, .. } => doc,
        }
    }

    pub(crate) fn new_token_range(doc: Doc, first: PToken, last: PToken) -> Self {
        assert!(first <= last);
        if first == last {
            return Loc::Token { doc, token: first };
        }

        Loc::TokenRange { doc, first, last }
    }

    pub(crate) fn unite(self, other: Loc) -> Self {
        if self.doc() != other.doc() {
            log::error!(
                "異なるドキュメントのロケーションを併合できません {:?}",
                (self, other)
            );
            return self;
        }

        match (self, other) {
            (Loc::Range { doc, range }, Loc::Range { range: other, .. }) => Loc::Range {
                doc,
                range: range.unite(&other),
            },
            (Loc::Token { doc, token, .. }, Loc::Token { token: other, .. }) => {
                Loc::new_token_range(doc, token.min(other), token.max(other))
            }
            (Loc::Token { doc, token, .. }, Loc::TokenRange { first, last, .. })
            | (Loc::TokenRange { doc, first, last }, Loc::Token { token, .. }) => {
                Loc::new_token_range(doc, first.min(token), last.max(token))
            }
            (
                Loc::TokenRange { doc, first, last },
                Loc::TokenRange {
                    first: other_first,
                    last: other_last,
                    ..
                },
            ) => Loc::new_token_range(doc, first.min(other_first), last.max(other_last)),
            _ => {
                log::error!("ロケーションを併合できません ({:?})", (self, other));
                self
            }
        }
    }
}

impl Debug for Loc {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        // Debug::fmt(&self.doc, f)?;

        // FIXME: base の位置情報
        // write!(f, " ")?;
        // Debug::fmt(&self.token, f)?;

        // match self.part {
        //     LocPart::Range => {}
        //     LocPart::Ahead => write!(f, ":ahead")?,
        //     LocPart::Behind => write!(f, ":behind")?,
        // }
        Ok(())
    }
}

impl Loc {
    pub(crate) fn behind(self) -> Self {
        match self {
            Loc::Range { doc, range } => Loc::Range {
                doc,
                range: range.behind(),
            },
            Loc::Token { doc, token } => Loc::TokenBehind { doc, token },
            Loc::TokenBehind { .. } => self,
            Loc::TokenRange { doc, last, .. } => Loc::TokenBehind { doc, token: last },
        }
    }
}

pub(crate) trait LocResolver {
    fn doc_path(&self, doc: Doc) -> Option<&Path>;

    fn token_range(&self, doc: Doc, token: PToken) -> TRange;
}
