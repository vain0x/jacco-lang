use super::{k_local::KLocalArena, k_ty::KTy2, KLocal};
use crate::{
    parse::{ANameKey, PLoc, PToken},
    source::Loc,
    source::{Doc, HaveLoc},
    utils::DebugWithContext,
};
use std::fmt::{self, Formatter};

#[derive(Copy, Clone, Debug)]
pub(crate) enum KSymbolCause {
    Loc(Loc),
    WildcardPat(Doc, PToken),
    NameDef(Doc, ANameKey),
    NameUse(Doc, ANameKey),
}

impl KSymbolCause {
    pub(crate) fn loc(self) -> Loc {
        match self {
            KSymbolCause::Loc(loc) => loc,
            KSymbolCause::WildcardPat(doc, token) => Loc::new(doc, PLoc::Token(token)),
            KSymbolCause::NameDef(doc, key) | KSymbolCause::NameUse(doc, key) => {
                Loc::new(doc, PLoc::Name(key))
            }
        }
    }
}

impl From<Loc> for KSymbolCause {
    fn from(loc: Loc) -> Self {
        KSymbolCause::Loc(loc)
    }
}

/// ローカル変数の出現
#[derive(Copy, Clone, Debug)]
pub(crate) struct KSymbol {
    pub(crate) local: KLocal,
    pub(crate) cause: KSymbolCause,
}

impl KSymbol {
    pub(crate) fn ty(&self, locals: &KLocalArena) -> KTy2 {
        self.local.ty(locals)
    }

    pub(crate) fn ty_mut<'a>(&mut self, locals: &'a mut KLocalArena) -> &'a mut KTy2 {
        &mut self.local.of_mut(locals).ty
    }

    pub(crate) fn loc(&self) -> Loc {
        self.cause.loc()
    }
}

impl HaveLoc for KSymbol {
    fn loc(&self) -> Loc {
        KSymbol::loc(self)
    }
}

impl DebugWithContext<KLocalArena> for KSymbol {
    fn fmt(&self, context: &KLocalArena, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (cause = {:?})",
            self.local.of(context).name,
            self.cause
        )
    }
}
