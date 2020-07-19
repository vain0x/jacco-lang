use super::{Doc, TRange};
use crate::parse::PToken;
use std::{fmt::Debug, path::Path};

/// 位置情報
///
/// QUESTION: 中間表現がトークンの番号を参照することの是非?
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Loc {
    Unknown(&'static str),
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

impl Default for Loc {
    fn default() -> Loc {
        Loc::Unknown("<Loc::default>")
    }
}

impl Loc {
    pub(crate) fn new_token_range(doc: Doc, first: PToken, last: PToken) -> Self {
        assert!(first <= last);
        if first == last {
            return Loc::Token { doc, token: first };
        }

        Loc::TokenRange { doc, first, last }
    }

    pub(crate) fn doc_opt(self) -> Result<Doc, &'static str> {
        match self {
            Loc::Unknown(hint) => Err(hint),
            Loc::Range { doc, .. }
            | Loc::Token { doc, .. }
            | Loc::TokenBehind { doc, .. }
            | Loc::TokenRange { doc, .. } => Ok(doc),
        }
    }

    pub(crate) fn doc(self) -> Doc {
        self.doc_opt().unwrap_or(Doc::MAX)
    }

    pub(crate) fn range_opt(self) -> Result<TRange, &'static str> {
        match self {
            Loc::Range { range, .. } => Ok(range),
            Loc::Unknown(hint) => Err(hint),
            Loc::Token { .. } | Loc::TokenBehind { .. } | Loc::TokenRange { .. } => {
                Err("<Loc::range_opt>")
            }
        }
    }

    pub(crate) fn unite(self, other: Loc) -> Self {
        if self.doc() != other.doc() {
            log::error!(
                "[BUG] 異なるドキュメントのロケーションを併合できません {:?}",
                (self, other)
            );
            return self;
        }

        match (self, other) {
            (Loc::Unknown(..), Loc::Unknown(..)) => self,
            (Loc::Unknown(..), other) | (other, Loc::Unknown(..)) => other,
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
            (Loc::Range { .. }, _)
            | (_, Loc::Range { .. })
            | (Loc::TokenBehind { .. }, _)
            | (_, Loc::TokenBehind { .. }) => {
                log::error!("ロケーションを併合できません ({:?})", (self, other));
                self
            }
        }
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
            Loc::TokenRange { doc, last, .. } => Loc::TokenBehind { doc, token: last },
            Loc::Unknown(..) | Loc::TokenBehind { .. } => self,
        }
    }
}

pub(crate) trait LocResolver {
    fn doc_path(&self, doc: Doc) -> Option<&Path>;

    fn token_range(&self, doc: Doc, token: PToken) -> TRange;
}
