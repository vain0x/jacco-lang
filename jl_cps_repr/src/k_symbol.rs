use super::{KConst, KExternFn, KFn, KLocal, KLocalData, KStaticVar, KStruct, KTy};
use crate::source::{HaveLocation, Location};

/// ローカル変数の出現
#[derive(Clone, Debug, Default)]
pub struct KSymbol {
    pub(crate) local: KLocal,
    pub(crate) location: Location,
}

impl KSymbol {
    pub fn local(&self) -> KLocal {
        self.local
    }

    pub fn name<'a>(&self, locals: &'a [KLocalData]) -> &'a str {
        self.local.name(locals)
    }

    pub fn ty(&self, locals: &[KLocalData]) -> KTy {
        self.local.ty(locals).to_owned()
    }

    pub(crate) fn ty_mut(self, locals: &mut [KLocalData]) -> &mut KTy {
        self.local.ty_mut(locals)
    }

    pub fn is_alive(&self, locals: &[KLocalData]) -> bool {
        locals[self.local.id()].is_alive
    }
}

impl HaveLocation for KSymbol {
    fn location(&self) -> Location {
        self.location.clone()
    }
}

/// 名前を解決した結果。
#[derive(Clone, Debug)]
pub(crate) enum KSymbolExt {
    Symbol(KSymbol),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct {
        k_struct: KStruct,
        location: Location,
    },
}

impl KSymbolExt {
    pub(crate) fn expect_symbol(self) -> KSymbol {
        match self {
            KSymbolExt::Symbol(symbol) => symbol,
            _ => unreachable!("{:?}", self),
        }
    }
}
