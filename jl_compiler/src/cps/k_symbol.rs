use super::{KExternFn, KFn, KLocal, KLocalData, KTy};
use crate::token::{HaveLocation, Location};

/// ローカル変数の出現
#[derive(Clone, Debug, Default)]
pub(crate) struct KSymbol {
    pub(crate) local: KLocal,
    pub(crate) location: Location,
}

impl KSymbol {
    pub(crate) fn ty(&self, locals: &[KLocalData]) -> KTy {
        self.local.ty(locals).to_owned()
    }

    pub(crate) fn ty_mut<'a>(&mut self, locals: &'a mut [KLocalData]) -> &'a mut KTy {
        self.local.ty_mut(locals)
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
    Fn(KFn),
    ExternFn(KExternFn),
}

impl KSymbolExt {
    pub(crate) fn expect_symbol(self) -> KSymbol {
        match self {
            KSymbolExt::Symbol(symbol) => symbol,
            _ => unreachable!("{:?}", self),
        }
    }
}
