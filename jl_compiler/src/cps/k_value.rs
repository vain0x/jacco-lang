use super::*;

/// モジュール内で定義・使用される、値を表すシンボル。
///
/// エイリアスとローカル変数は除く。
pub(crate) enum KModValue {
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct(KStruct),
}

impl KModValue {
    pub(crate) fn to_local_value(self) -> KLocalValue {
        match self {
            KModValue::Const(it) => KLocalValue::Const(it),
            KModValue::StaticVar(it) => KLocalValue::StaticVar(it),
            KModValue::Fn(it) => KLocalValue::Fn(it),
            KModValue::ExternFn(it) => KLocalValue::ExternFn(it),
            KModValue::UnitLikeStruct(it) => KLocalValue::UnitLikeStruct(it),
        }
    }
}

/// モジュール内で定義・使用される、値を表すシンボル
pub(crate) enum KLocalValue {
    LocalVar(KLocalVar),
    Const(KConst),
    StaticVar(KStaticVar),
    Fn(KFn),
    ExternFn(KExternFn),
    UnitLikeStruct(KStruct),
}

pub(crate) enum KValueOrAlias {
    Alias(KAlias),
    Value(KModValue),
}
