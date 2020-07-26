use super::{
    k_local::KLocalArena, k_ty::KTy2, KAlias, KConst, KExternFn, KFn, KLocal, KStaticVar, KStruct,
};
use crate::{source::HaveLoc, source::Loc, utils::DebugWithContext};
use std::fmt::{self, Formatter};

/// ローカル変数の出現
#[derive(Copy, Clone, Debug)]
pub(crate) struct KSymbol {
    pub(crate) local: KLocal,
    pub(crate) loc: Loc,
}

impl KSymbol {
    pub(crate) fn ty(&self, locals: &KLocalArena) -> KTy2 {
        self.local.ty(locals)
    }

    pub(crate) fn ty_mut<'a>(&mut self, locals: &'a mut KLocalArena) -> &'a mut KTy2 {
        &mut self.local.of_mut(locals).ty
    }
}

impl HaveLoc for KSymbol {
    fn loc(&self) -> Loc {
        self.loc
    }
}

impl DebugWithContext<KLocalArena> for KSymbol {
    fn fmt(&self, context: &KLocalArena, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} (loc = {:?})", self.local.of(context).name, self.loc)
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
