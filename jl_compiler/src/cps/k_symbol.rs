use super::{
    k_local::KLocalArena, k_ty::KTy2, KAlias, KConst, KExternFn, KFn, KLocal, KStaticVar, KStruct,
};
use crate::{source::HaveLocation, source::Loc};

/// ローカル変数の出現
#[derive(Clone, Debug)]
pub(crate) struct KSymbol {
    pub(crate) local: KLocal,
    pub(crate) location: Loc,
}

impl KSymbol {
    pub(crate) fn ty(&self, locals: &KLocalArena) -> KTy2 {
        self.local.ty(locals)
    }

    pub(crate) fn ty_mut<'a>(&mut self, locals: &'a mut KLocalArena) -> &'a mut KTy2 {
        &mut self.local.of_mut(locals).ty
    }
}

impl HaveLocation for KSymbol {
    fn location(&self) -> Loc {
        self.location
    }
}

/// 名前を解決した結果。
#[derive(Clone, Debug)]
pub(crate) enum KSymbolExt {
    Unresolved,
    Alias { alias: KAlias, location: Loc },
    Symbol(KSymbol),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct { k_struct: KStruct, location: Loc },
}

impl KSymbolExt {
    pub(crate) fn as_symbol(self) -> Option<KSymbol> {
        match self {
            KSymbolExt::Symbol(symbol) => Some(symbol),
            _ => None,
        }
    }
}
