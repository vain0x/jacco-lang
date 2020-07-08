use super::{KConst, KExternFn, KFn, KLocal, KLocalData, KStaticVar, KStruct, KTy};
use crate::token::{HaveLocation, Location, TokenSource};

/// ローカル変数の出現
#[derive(Clone, Debug)]
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

impl Default for KSymbol {
    fn default() -> Self {
        Self {
            local: Default::default(),
            location: Location::new(
                TokenSource::Special("<KSymbol::default>"),
                Default::default(),
            ),
        }
    }
}

impl HaveLocation for KSymbol {
    fn location(&self) -> Location {
        self.location
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
