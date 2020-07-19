use super::{Doc, TRange};
use crate::{
    parse::PToken,
    token::{TokenData, TokenSource},
    utils::TakeOut,
};
use std::{fmt::Debug, mem::replace, path::Path};

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
    #[allow(unused)]
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
    pub(crate) fn new(source: TokenSource, range: TRange) -> Loc {
        match source {
            TokenSource::Special(name) => Loc::Unknown(name),
            TokenSource::File(doc) => Loc::Range { doc, range },
        }
    }

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

    pub(crate) fn range(self) -> TRange {
        self.range_opt().unwrap_or(TRange::ZERO)
    }

    #[allow(unused)]
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

impl Default for Loc {
    fn default() -> Loc {
        Loc::Unknown("<Loc::default>")
    }
}

impl TakeOut for Loc {
    fn take_out(&mut self) -> Loc {
        replace(self, Loc::Unknown("<Loc::take_out>"))
    }
}

pub(crate) trait LocResolver {
    fn doc_path(&self, doc: Doc) -> Option<&Path>;

    fn token_range(&self, doc: Doc, token: PToken) -> TRange;
}

pub(crate) trait HaveLocation {
    fn location(&self) -> Loc;
}

impl HaveLocation for (Doc, TRange) {
    fn location(&self) -> Loc {
        let (doc, range) = *self;
        Loc::Range { doc, range }
    }
}

impl<'a, T: HaveLocation> HaveLocation for &'a T {
    fn location(&self) -> Loc {
        T::location(*self)
    }
}

impl<'a, T: HaveLocation> HaveLocation for &'a mut T {
    fn location(&self) -> Loc {
        T::location(*self)
    }
}

impl HaveLocation for Loc {
    fn location(&self) -> Loc {
        *self
    }
}

impl HaveLocation for TokenData {
    fn location(&self) -> Loc {
        TokenData::location(self)
    }
}
