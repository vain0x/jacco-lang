use super::{k_local_var::KLocalVarArena, k_ty::KTy2, KLocalVar};
use crate::{
    parse::{ANameId, PLoc, PToken},
    source::Loc,
    source::{Doc, HaveLoc},
    utils::DebugWithContext,
};
use std::fmt::{self, Formatter};

#[derive(Copy, Clone, Debug)]
pub(crate) enum KVarTermCause {
    Loc(Loc),
    WildcardPat(Doc, PToken),
    NameDef(Doc, ANameId),
    NameUse(Doc, ANameId),
}

impl KVarTermCause {
    pub(crate) fn loc(self) -> Loc {
        match self {
            KVarTermCause::Loc(loc) => loc,
            KVarTermCause::WildcardPat(doc, token) => Loc::new(doc, PLoc::Token(token)),
            KVarTermCause::NameDef(doc, key) | KVarTermCause::NameUse(doc, key) => {
                Loc::new(doc, PLoc::Name(key))
            }
        }
    }
}

impl From<Loc> for KVarTermCause {
    fn from(loc: Loc) -> Self {
        KVarTermCause::Loc(loc)
    }
}

/// ローカル変数の出現
#[derive(Copy, Clone, Debug)]
pub(crate) struct KVarTerm {
    pub(crate) local_var: KLocalVar,
    pub(crate) cause: KVarTermCause,
}

impl KVarTerm {
    pub(crate) fn ty(&self, local_vars: &KLocalVarArena) -> KTy2 {
        self.local_var.ty(local_vars)
    }

    pub(crate) fn ty_mut(self, local_vars: &mut KLocalVarArena) -> &mut KTy2 {
        &mut self.local_var.of_mut(local_vars).ty
    }

    pub(crate) fn loc(&self) -> Loc {
        self.cause.loc()
    }
}

impl HaveLoc for KVarTerm {
    fn loc(&self) -> Loc {
        KVarTerm::loc(self)
    }
}

impl DebugWithContext<KLocalVarArena> for KVarTerm {
    fn fmt(&self, context: &KLocalVarArena, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (cause = {:?})",
            self.local_var.of(context).name,
            self.cause
        )
    }
}
