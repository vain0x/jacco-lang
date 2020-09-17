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
