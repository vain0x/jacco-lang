use super::{
    k_local::KLocalArena, k_ty::KTy2, KAlias, KConst, KExternFn, KFn, KLocal, KStaticVar, KStruct,
};
use crate::{
    parse::{ADeclId, ANameKey, AParamDeclKey, PLoc},
    source::Loc,
    source::{Doc, HaveLoc},
    utils::DebugWithContext,
};
use std::fmt::{self, Formatter};

#[derive(Copy, Clone, Debug)]
pub(crate) enum KSymbolCause {
    Loc(Loc),
    LetDecl {
        doc: Doc,
        decl_id: ADeclId,
    },
    ParamDecl {
        doc: Doc,
        decl_id: ADeclId,
        index: usize,
    },
}

impl KSymbolCause {
    pub(crate) fn loc(self) -> Loc {
        match self {
            KSymbolCause::Loc(loc) => loc,
            KSymbolCause::LetDecl { doc, decl_id } => {
                Loc::new(doc, PLoc::Name(ANameKey::Decl(decl_id)))
            }
            KSymbolCause::ParamDecl {
                doc,
                decl_id,
                index,
            } => Loc::new(
                doc,
                PLoc::Name(ANameKey::Param(AParamDeclKey::new(decl_id, index))),
            ),
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

/// 名前を解決した結果。
#[derive(Clone, Debug)]
pub(crate) enum KSymbolExt {
    Unresolved,
    Alias { alias: KAlias, loc: Loc },
    Symbol(KSymbol),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct { k_struct: KStruct, loc: Loc },
}

impl KSymbolExt {
    pub(crate) fn as_symbol(self) -> Option<KSymbol> {
        match self {
            KSymbolExt::Symbol(symbol) => Some(symbol),
            _ => None,
        }
    }
}
