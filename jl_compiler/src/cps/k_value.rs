use super::*;

/// モジュール内で定義・使用される、値を表すシンボル
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum KLocalValue {
    Alias(KAlias),
    LocalVar(KLocalVar),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct(KStruct),
}

/// プロジェクト内のモジュールで定義・使用される、値を表すシンボル
#[derive(Copy, Clone)]
pub(crate) struct KProjectValue {
    #[allow(unused)]
    pub(crate) k_mod: KMod,
    // FIXME: 外部のモジュールのローカル変数やエイリアスを指してはいけない
    pub(crate) value: KLocalValue,
}

impl KProjectValue {
    pub(crate) fn new(k_mod: KMod, value: KLocalValue) -> Self {
        Self { k_mod, value }
    }
}
