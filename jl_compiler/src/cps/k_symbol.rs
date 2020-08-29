use super::{k_local_var::KLocalVarArena, k_ty::KTy2, KLocalVar};
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
    pub(crate) local_var: KLocalVar,
    pub(crate) cause: KSymbolCause,
}

impl KSymbol {
    pub(crate) fn ty(&self, local_vars: &KLocalVarArena) -> KTy2 {
        self.local_var.ty(local_vars)
    }

    pub(crate) fn ty_mut<'a>(&mut self, local_vars: &'a mut KLocalVarArena) -> &'a mut KTy2 {
        &mut self.local_var.of_mut(local_vars).ty
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

impl DebugWithContext<KLocalVarArena> for KSymbol {
    fn fmt(&self, context: &KLocalVarArena, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (cause = {:?})",
            self.local_var.of(context).name,
            self.cause
        )
    }
}
